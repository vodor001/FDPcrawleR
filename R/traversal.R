# Compile explicit structural rules; this is not a SHACL validator or reasoner.
compile_traversal <- function(spec = NULL) {
  if (is.null(spec)) spec <- list(extends = "standard-fdp-dcat")
  if (!is.list(spec) || any(!names(spec) %in% c("extends", "resource_types", "relationships", "infer_types_from_url"))) {
    stop("Unknown traversal option; use extends, resource_types, relationships and infer_types_from_url.")
  }
  infer_url <- spec$infer_types_from_url %||% FALSE
  if (!is.logical(infer_url) || length(infer_url) != 1L || is.na(infer_url)) stop("Invalid infer_types_from_url flag.")
  if (is.null(spec$extends)) spec$extends <- "standard-fdp-dcat"
  if (!identical(spec$extends, "standard-fdp-dcat")) stop("Unknown traversal base.")
  base <- yaml::read_yaml("config/traversal-standard.yml")
  custom <- spec$resource_types
  if (length(custom) && (is.null(names(custom)) || anyDuplicated(names(custom)) ||
      any(!grepl("^[a-z][a-z0-9_]*$", names(custom))))) stop("Resource types need unique lowercase identifiers.")
  for (id in names(custom)) {
    value <- custom[[id]]
    if (!is.list(value) || any(!names(value) %in% c("rdf_types", "label", "behaves_like", "terminal"))) {
      stop("Invalid resource type: ", id)
    }
    base$resource_types[[id]] <- utils::modifyList(base$resource_types[[id]] %||% list(), value)
  }
  types <- base$resource_types
  ancestors <- function(id) {
    chain <- character()
    while (!is.null(id)) {
      if (!is.character(id) || length(id) != 1L || !id %in% names(types)) stop("Unknown behaves_like type.")
      if (id %in% chain) stop("Cycle in behaves_like: ", id)
      chain <- c(chain, id)
      id <- types[[id]]$behaves_like
    }
    chain
  }
  for (id in names(types)) {
    chain <- ancestors(id)
    types[[id]]$ancestors <- chain
    types[[id]]$label <- types[[id]]$label %||% id
    if (!is.character(types[[id]]$label) || length(types[[id]]$label) != 1L ||
        is.na(types[[id]]$label) || !nzchar(types[[id]]$label)) stop("Invalid type label: ", id)
    rdf <- unlist(types[[id]]$rdf_types, use.names = FALSE)
    if (length(rdf) && (!is.character(rdf) || anyNA(rdf) || any(!grepl("^[A-Za-z][A-Za-z0-9+.-]*:", rdf)))) {
      stop("rdf_types must contain absolute IRIs: ", id)
    }
    terminals <- lapply(chain, function(x) base$resource_types[[x]]$terminal)
    terminals <- Filter(Negate(is.null), terminals)
    terminal <- if (length(terminals)) terminals[[1]] else FALSE
    if (!is.logical(terminal) || length(terminal) != 1L || is.na(terminal)) stop("Invalid terminal flag: ", id)
    types[[id]]$terminal <- terminal
  }
  relations <- c(base$relationships, spec$relationships)
  for (rule in relations) {
    if (!is.list(rule) || !setequal(names(rule), c("from", "predicate", "to")) ||
        any(!vapply(rule, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))) ||
        !rule$from %in% names(types) || !rule$to %in% names(types) ||
        !grepl("^[A-Za-z][A-Za-z0-9+.-]*:", rule$predicate)) stop("Invalid traversal relationship.")
  }
  rules <- unique(as.data.frame(do.call(rbind, lapply(relations, function(x) unlist(x[c("from", "predicate", "to")]))), stringsAsFactors = FALSE))
  for (id in names(types)) {
    if (types[[id]]$terminal && any(rules$from %in% types[[id]]$ancestors)) {
      stop("Terminal type has outgoing relationships: ", id, ". Set terminal: false to extend it.")
    }
  }
  list(types = types, rules = rules, infer_types_from_url = infer_url)
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x

# Queue entries retain owner and discovery role. Documents are cached separately
# by the crawler: seeing a downloaded URL is not evidence it has been expanded.
traverse_metadata <- function(roots, spec, fetch, subject_for, compare, url_type) {
  model <- compile_traversal(spec)
  types <- model$types
  rules <- model$rules
  rdf_type <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  ldp <- "http://www.w3.org/ns/ldp#"
  norm <- function(x) sub("/+$", "", trimws(x))
  nodes <- edges <- cases <- extras <- list()
  queue <- lapply(roots, function(url) list(fdp = url, url = url, role = "fdp", candidates = "fdp"))
  scheduled <- new.env(parent = emptyenv())
  # Separate environments: a scheduled resource still needs to be expanded.
  expanded <- new.env(parent = emptyenv())
  key <- function(owner, url, role) paste(owner, url, role, sep = "\n")
  for (item in queue) assign(key(item$fdp, item$url, paste(item$role, paste(item$candidates, collapse = ","))), TRUE, scheduled)
  enqueue <- function(owner, url, role, candidates = role) {
    k <- key(owner, url, paste(role, paste(sort(candidates), collapse = ",")))
    if (!exists(k, scheduled, inherits = FALSE)) {
      assign(k, TRUE, scheduled)
      queue[[length(queue) + 1L]] <<- list(fdp = owner, url = url, role = role, candidates = candidates)
    }
  }
  cursor <- 1L
  while (cursor <= length(queue)) {
    item <- queue[[cursor]]
    cursor <- cursor + 1L
    doc <- fetch(item$url, types[[item$role]]$label %||% "Resource classification")
    subject <- subject_for(doc, item$url, unique(rules$predicate),
                           unlist(lapply(types, `[[`, "rdf_types"), use.names = FALSE))
    local_types <- doc$triples$o[doc$triples$s %in% subject & doc$triples$p == rdf_type]
    matched <- names(types)[vapply(types, function(x) any(local_types %in% unlist(x$rdf_types)), logical(1))]
    # RDF classes refine the permitted destination; they cannot introduce a
    # different route (for example Catalog -> DataService in the generic base).
    compatible <- names(types)[vapply(types, function(x) any(x$ancestors %in% item$candidates), logical(1))]
    matched <- intersect(matched, compatible)
    roles <- unique(c(if (item$role != "resource") item$role, matched))
    if (model$infer_types_from_url) {
      inferred <- tolower(url_type(item$url))
      if (inferred %in% compatible && inferred != "resource" &&
          (!length(roles) || any(types[[inferred]]$ancestors %in% roles))) roles <- unique(c(roles, inferred))
    }
    if (!length(roles)) roles <- "resource"
    # Prefer a configured specialization over its ancestor. Its inherited rules
    # still apply, without counting the same structural warning twice when RDF
    # declares both the custom class and dcat:Dataset.
    roles <- roles[!vapply(roles, function(role) any(vapply(setdiff(roles, role),
      function(other) role %in% types[[other]]$ancestors, logical(1))), logical(1))]
    for (role in roles) {
      k <- key(item$fdp, item$url, role)
      if (exists(k, expanded, inherits = FALSE)) next
      assign(k, TRUE, expanded)
      applicable <- rules[rules$from %in% types[[role]]$ancestors, , drop = FALSE]
      # Compare all recognized roles together so one role's child is not recorded
      # as an unsupported relationship by another role on the same document.
      allowed <- rules[rules$from %in% unique(unlist(lapply(roles, function(r) types[[r]]$ancestors))), , drop = FALSE]
      status <- if (doc$check_status != "ok") doc$check_status else
        if (!nrow(applicable) && !types[[role]]$terminal) "unsupported_type" else "expanded"
      nodes[[length(nodes) + 1L]] <- data.frame(fdp = item$fdp, url = item$url,
        document_url = doc$document_url, resource_type = role, rdf_types = paste(local_types, collapse = "; "),
        traversal_status = status, terminal = types[[role]]$terminal)
      if (doc$check_status != "ok") next
      checked <- compare(doc, subject, unique(allowed$predicate))
      # Preserve unknown scoped LDP membership; never guess arbitrary RDF links.
      t <- doc$triples
      membership <- t[t$p == paste0(ldp, "membershipResource") & norm(t$o) %in% norm(subject), , drop = FALSE]
      for (container in unique(membership$s)) {
        predicates <- t$o[t$s == container & t$p == paste0(ldp, "hasMemberRelation")]
        for (predicate in setdiff(predicates, allowed$predicate)) {
          children <- t$o[t$s == container & t$p == paste0(ldp, "contains") & grepl("^https?://", t$o)]
          extras[[length(extras) + 1L]] <- data.frame(parent_url = item$url, relation = predicate,
            child_url = if (length(children)) children else NA_character_, container_url = container,
            traversal_status = "not traversed")
        }
      }
      if (status != "expanded" || types[[role]]$terminal) next
      add_case <- function(case, child = NA_character_, container = NA_character_) {
        cases[[length(cases) + 1L]] <<- data.frame(fdp = item$fdp, `Metadata Layer` = types[[role]]$label,
          case = case, parent_url = item$url, child_url = child, container_url = container,
          check_status = "ok", error = NA_character_, check.names = FALSE)
      }
      for (container in checked$containers$container_url[checked$containers$n_contains == 0L]) {
        add_case("Empty LDP containers", container = container)
      }
      if (!checked$has_metadata_links && !nrow(checked$containers) && !length(
          t$o[t$s %in% membership$s & t$p == paste0(ldp, "hasMemberRelation")])) {
        add_case("No DCAT or LDP continuation")
      }
      for (i in seq_len(nrow(checked$comparison))) {
        edge <- checked$comparison[i, ]
        predicate <- edge$listed_predicate
        if (is.na(predicate)) {
          predicate <- checked$containers$member_relation[checked$containers$container_url %in% edge$container_url]
          predicate <- predicate[!is.na(predicate)]
        }
        selected <- applicable[applicable$predicate %in% predicate, , drop = FALSE]
        if (!length(predicate) && nrow(applicable)) {
          # Bare contains is evidence of containment, not of every possible
          # domain predicate. Let the child RDF classify ambiguous destinations.
          targets <- unique(applicable$to)
          selected <- data.frame(from = role, predicate = paste0(ldp, "contains"),
            to = if (length(targets) == 1L) targets else "resource")
        }
        for (j in seq_len(nrow(selected))) {
          rule <- selected[j, ]
          edges[[length(edges) + 1L]] <- data.frame(fdp = item$fdp, parent_url = item$url,
            parent_type = role, relation = rule$predicate, child_url = edge$resource_url,
            child_type = rule$to, container_url = edge$container_url,
            consistency_status = edge$consistency_status)
          candidates <- if (rule$to == "resource") setdiff(unique(applicable$to), "resource") else rule$to
          enqueue(item$fdp, edge$resource_url, rule$to, candidates)
        }
        if (nrow(selected) && edge$consistency_status == "container_without_listing") {
          add_case("LDP-only links", child = edge$resource_url)
        }
      }
    }
  }
  bind <- function(rows, template) dplyr::distinct(dplyr::bind_rows(template, rows))
  result <- list(documents = bind(nodes, tibble::tibble(fdp = character(), url = character(), document_url = character(),
    resource_type = character(), rdf_types = character(), traversal_status = character(), terminal = logical())),
    edges = bind(edges, tibble::tibble(fdp = character(), parent_url = character(), parent_type = character(),
      relation = character(), child_url = character(), child_type = character(), container_url = character(), consistency_status = character())),
    cases = bind(cases, tibble::tibble(fdp = character(), `Metadata Layer` = character(), case = character(),
      parent_url = character(), child_url = character(), container_url = character(), check_status = character(), error = character())),
    additional = bind(extras, tibble::tibble(parent_url = character(), relation = character(), child_url = character(),
      container_url = character(), traversal_status = character())))
  # A generic discovery can precede a second link specifying the actual role.
  known <- result$documents[result$documents$traversal_status == "expanded", c("fdp", "url")]
  resolved <- paste(result$documents$fdp, result$documents$url) %in% paste(known$fdp, known$url)
  result$documents <- result$documents[!(resolved & result$documents$resource_type == "resource" &
    result$documents$traversal_status == "unsupported_type"), , drop = FALSE]
  result$additional <- dplyr::anti_join(result$additional,
    dplyr::distinct(result$edges, parent_url, relation, child_url), by = c("parent_url", "relation", "child_url"))
  result
}
