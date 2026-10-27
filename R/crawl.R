suppressPackageStartupMessages(library(dplyr))
source("R/traversal.R", local = TRUE)

# Crawl FDP -> Catalog -> Resource -> Distribution -> DataService metadata.
# Dataset -> DataService and Catalog -> DataService are also supported.
# fdp_list: starting FDP URLs; timeout_seconds: timeout for each default HTTP request.
# fetch_fun / endpoint_fetch_fun: optional request functions for offline tests.
# Returns tables, discovered URLs, document checks, and the combined RDF graph.
# Each call has its own graph/cache; active and inactive runs remain independent.
crawl_fdp_clean <- function(fdp_list, timeout_seconds = 10, fetch_fun = NULL,
                            endpoint_fetch_fun = NULL, generalized = FALSE, traversal = NULL) {
  # 1. Shared helpers and RDF predicates.
  # Normalisation removes trailing slashes for comparisons, not schemes or ports.
  # Triple columns s, p, o mean subject, predicate, and object respectively.
  norm <- function(x) sub("/+$", "", trimws(as.character(x)))
  urls <- function(x) unique(x[!is.na(x) & grepl("^https?://", x)])
  empty <- function(...) tibble::tibble(...)
  triples_empty <- empty(s = character(), p = character(), o = character())
  rdf_type <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  model <- if (generalized || !is.null(traversal)) compile_traversal(traversal) else NULL
  infer_url_types <- is.null(model) || model$infer_types_from_url
  ldp <- "http://www.w3.org/ns/ldp#"
  dcat <- "http://www.w3.org/ns/dcat#"
  metadata_catalog <- "https://w3id.org/fdp/fdp-o#metadataCatalog"
  has_part <- "http://purl.org/dc/terms/hasPart"
  P_DCAT_DISTRIBUTION <- paste0(dcat, "distribution")
  P_DCAT_DATASERVICE <- paste0(dcat, "accessService")
  P_DCAT_ENDPOINT <- paste0(dcat, "endpointURL")
  # The graph combines metadata for export. The cache retains each document's
  # own triples so shared container URLs cannot mix contents between catalogs.
  graph <- rdflib::rdf()
  cache <- new.env(parent = emptyenv())
  fetch_log <- list()
  container_checks <- empty(parent_url = character(), level = character(),
    container_url = character(), membership_resource = character(),
    member_relation = character(), n_contains = integer())
  additional_relationships <- empty(parent_url = character(), relation = character(),
    child_url = character(), container_url = character(), traversal_status = character())
  continuation_checks <- empty(parent_url = character(), level = character(),
    has_metadata_links = logical(), n_containers = integer())

  # Resolve relative RDF URLs against the fetched document URL and report syntax
  # errors explicitly, rather than accepting a partially parsed Turtle document.
  parse_turtle <- function(text, base, target = rdflib::rdf()) {
    # rdflib::rdf_parse() discards Redland's nonzero parse status in this version.
    parser <- methods::new("Parser", target$world, name = "turtle", mimeType = "text/turtle")
    on.exit(redland::freeParser(parser), add = TRUE)
    base_uri <- redland::librdf_new_uri(target$world@librdf_world, base)
    on.exit(redland::librdf_free_uri(base_uri), add = TRUE)
    status <- redland::librdf_parser_parse_string_into_model(
      parser@librdf_parser, text, base_uri, target$model@librdf_model)
    if (status != 0L) stop("Invalid Turtle (parser status ", status, ")")
    target
  }

  # 2. HTTP requests and cached document parsing.
  # Endpoints may return any content type. Metadata requests ask for Turtle.
  # An injected metadata fetcher also handles endpoints unless overridden.
  if (is.null(endpoint_fetch_fun)) {
    endpoint_fetch_fun <- if (!is.null(fetch_fun)) fetch_fun else function(url) {
      response <- httr::GET(url, httr::timeout(timeout_seconds),
                            httr::add_headers(Accept = "*/*"))
      list(status_code = httr::status_code(response), url = response$url)
    }
  }
  if (is.null(fetch_fun)) {
    fetch_fun <- function(url) {
      response <- httr::GET(url, httr::timeout(timeout_seconds),
                            httr::add_headers(Accept = "text/turtle"))
      list(body = httr::content(response, as = "text", encoding = "UTF-8"),
           status_code = httr::status_code(response), url = response$url)
    }
  }

  fetch_document <- function(url, level) {
    # Reuse successful AND failed attempts for this exact URL within the run.
    # A cached return adds neither another request nor another fetch_log entry.
    if (exists(url, cache, inherits = FALSE)) return(get(url, cache))
    message("Processing ", level, ": ", url)
    result <- list(requested_url = url, triples = triples_empty, turtle_text = NA_character_,
                   document_url = url, http_status = NA_integer_,
                   check_status = "fetch_failed", error = NA_character_)
    response <- tryCatch(fetch_fun(url), error = function(e) e)
    if (inherits(response, "error")) {
      result$error <- conditionMessage(response)
    } else {
      result$http_status <- as.integer(response$status_code)
      if (!is.null(response$url)) result$document_url <- response$url
      if (is.na(result$http_status) || result$http_status >= 400L) {
        result$error <- paste("HTTP", result$http_status)
      } else if (is.null(response$body) || is.na(response$body) || !nzchar(response$body)) {
        result$error <- "Empty response body"
      } else {
        result$turtle_text <- response$body
        parsed <- tryCatch({
          # Parse locally for comparisons, then parse the same downloaded text
          # into the combined graph. This is one fetch but two parsing operations.
          local_graph <- parse_turtle(response$body, result$document_url)
          triples <- rdflib::rdf_query(local_graph, "SELECT ?s ?p ?o WHERE { ?s ?p ?o }")
          if (nrow(triples) == 0L) triples <- triples_empty
          triples <- as_tibble(triples) %>%
            mutate(across(c(s, p, o), as.character)) %>% distinct()
          graph <<- parse_turtle(response$body, result$document_url, graph)
          triples
        }, error = function(e) e)
        if (inherits(parsed, "error")) {
          result$check_status <- "parse_failed"
          result$error <- conditionMessage(parsed)
        } else {
          result$triples <- parsed
          result$check_status <- "ok"
        }
      }
    }
    # check_status distinguishes fetch_failed, parse_failed, and ok. The log's
    # level records the URL's first fetch role, not every path that discovers it.
    if (result$check_status != "ok") message("  ", result$check_status, ": ", result$error)
    fetch_log[[length(fetch_log) + 1L]] <<- empty(
      level = level, url = url, document_url = result$document_url,
      http_status = result$http_status, check_status = result$check_status, error = result$error)
    assign(url, result, cache)
    result
  }

  # Resolve a document's published root without globally stripping ports or schemes.
  # Prefer requested/redirected URL matches, then an unambiguous typed subject
  # or relevant-predicate subject; do not select an arbitrary embedded resource.
  root_subject <- function(doc, requested, predicates, classes) {
    t <- doc$triples
    candidates <- unique(t$s[t$p %in% predicates |
                              (t$p == rdf_type & t$o %in% classes)])
    aliases <- norm(c(requested, sub("[?#].*$", "", doc$document_url)))
    exact <- unique(t$s[norm(t$s) %in% aliases])
    if (length(exact)) return(exact)
    typed <- unique(t$s[t$p == rdf_type & t$o %in% classes])
    if (length(typed) == 1L) return(typed)
    if (length(candidates) == 1L) return(candidates)
    requested
  }

  # Infer resource type from its URL path (e.g. /biobank/id), not from rdf:type.
  # URLs without a recognisable parent path segment receive "unknown".
  type_from_url <- function(x) {
    path <- sub("[?#].*$", "", x)
    path <- sub("^https?://[^/]+/?", "", path)
    path <- sub("/+$", "", path)
    as.character(ifelse(grepl("/", path), sub("^.*/([^/]+)/[^/]+$", "\\1", paste0("/", path)), "unknown"))
  }

  # 3. Compare explicit metadata links with LDP container contents.
  # Only compare triples in this document. Explicit membership metadata scopes
  # containers to this parent; other member relations are not distribution links.
  compare_document <- function(doc, subjects, predicates) {
    t <- doc$triples
    # "listed" is the parent subject's explicit child links, such as
    # dcat:dataset, dcat:distribution, or dcat:accessService.
    listed <- t %>% filter(norm(s) %in% norm(subjects), p %in% predicates,
                           grepl("^https?://", o)) %>%
      transmute(resource_url = o, listed_predicate = p) %>% distinct()
    # Some FDPs publish contains/membership triples without a container rdf:type.
    # Keep those discoveries, as in the original catalogs_not_linked_to_FDP check.
    containers <- t %>% filter(
      (p == rdf_type & o %in% paste0(ldp, c("DirectContainer", "BasicContainer", "Container"))) |
        p %in% paste0(ldp, c("contains", "membershipResource", "hasMemberRelation"))) %>%
      transmute(container_url = s) %>% distinct()
    memberships <- t %>% filter(p == paste0(ldp, "membershipResource")) %>%
      transmute(container_url = s, membership_resource = o) %>% distinct()
    relations <- t %>% filter(p == paste0(ldp, "hasMemberRelation")) %>%
      transmute(container_url = s, member_relation = o) %>% distinct()
    if (generalized) {
      # Preserve scoped extension relationships without interpreting them as
      # dcat:accessService, or following arbitrary publisher/license links.
      extra <- containers %>% inner_join(memberships, by = "container_url") %>%
        inner_join(relations, by = "container_url") %>%
        filter(norm(membership_resource) %in% norm(subjects),
               !member_relation %in% predicates) %>%
        left_join(t %>% filter(p == paste0(ldp, "contains"), grepl("^https?://", o)) %>%
          transmute(container_url = s, child_url = o), by = "container_url") %>%
        transmute(parent_url = membership_resource, relation = member_relation,
          child_url, container_url, traversal_status = "not traversed") %>% distinct()
      if (nrow(extra)) {
        # Store requested document ownership, including redirected subject aliases.
        extra$parent_url <- doc$requested_url
        additional_relationships <<- bind_rows(additional_relationships, extra) %>% distinct()
      }
    }
    # Respect membershipResource and hasMemberRelation when supplied. Missing
    # membership metadata is tolerated; explicit conflicting values are excluded.
    containers <- containers %>%
      left_join(memberships, by = "container_url") %>%
      left_join(relations, by = "container_url", relationship = "many-to-many") %>%
      filter(is.na(membership_resource) | norm(membership_resource) %in% norm(subjects),
             is.na(member_relation) | member_relation %in% predicates) %>% distinct()
    contents <- t %>% filter(p == paste0(ldp, "contains"), grepl("^https?://", o)) %>%
      transmute(container_url = s, resource_url = o) %>%
      semi_join(containers, by = "container_url") %>% distinct()
    # A full join preserves children found on only one side. Statuses describe
    # listing agreement, not HTTP reachability or formal DCAT validity.
    # listed_in_catalog is a legacy column name reused for any parent layer.
    comparison <- listed %>%
      full_join(contents, by = "resource_url", relationship = "many-to-many") %>%
      mutate(resource_type = if (infer_url_types) type_from_url(resource_url) else "unknown",
             listed_in_catalog = !is.na(listed_predicate),
             present_in_container = !is.na(container_url),
             consistency_status = case_when(
               listed_in_catalog & present_in_container ~ "matched",
               listed_in_catalog ~ "listed_without_container",
               TRUE ~ "container_without_listing")) %>% distinct()
    # Count all contains objects here, not just the HTTP(S) children we can crawl.
    # A container is empty only when it has no contains triples at all.
    containers <- containers %>%
      left_join(t %>% filter(p == paste0(ldp, "contains")) %>%
        distinct(container_url = s, o) %>% count(container_url, name = "n_contains"),
        by = "container_url") %>%
      mutate(n_contains = coalesce(n_contains, 0L))
    list(listed = listed, contents = contents, containers = containers, comparison = comparison,
         has_metadata_links = any(norm(t$s) %in% norm(subjects) & t$p %in% predicates))
  }

  record_containers <- function(checked, parent_url, level) {
    # Keep the parent document as well as the container URL: the same container
    # URL can appear empty in one catalog's Turtle and populated in another's.
    container_checks <<- bind_rows(container_checks, checked$containers %>%
      mutate(parent_url = .env$parent_url, level = .env$level, .before = 1)) %>% distinct()
    # Also retain successfully inspected parents that have no containers at all.
    continuation_checks <<- bind_rows(continuation_checks, empty(
      parent_url = parent_url, level = level, has_metadata_links = checked$has_metadata_links,
      n_containers = n_distinct(checked$containers$container_url))) %>% distinct()
  }

  # Count distinct child URLs, not join rows from multiple predicates/containers.
  count_comparison <- function(x) {
    empty(n_listed_resources = n_distinct(x$resource_url[x$listed_in_catalog]),
          n_container_resources = n_distinct(x$resource_url[x$present_in_container]),
          n_matched = n_distinct(x$resource_url[x$consistency_status == "matched"]),
          n_listed_without_container = n_distinct(x$resource_url[x$consistency_status == "listed_without_container"]),
          n_container_without_listing = n_distinct(x$resource_url[x$consistency_status == "container_without_listing"]))
  }

  # All pipeline runs use the queue. The remaining layer-specific code builds
  # the established standard-route report from the same cached documents.
  # Direct legacy calls without generalized/traversal retain their old contract.
  traversal_result <- NULL
  if (generalized || !is.null(traversal)) {
    traversal_result <- traverse_metadata(urls(norm(fdp_list)), traversal,
      fetch_document, root_subject, compare_document, type_from_url)
  }
  projection_targets <- function(parent, target) {
    if (is.null(traversal_result)) return(NULL)
    roles <- names(model$types)[vapply(model$types, function(x) target %in% x$ancestors, logical(1))]
    edges <- traversal_result$edges %>% filter(parent_url == parent)
    resolved <- traversal_result$documents %>% filter(resource_type %in% roles) %>% pull(url)
    unique(edges$child_url[edges$child_type %in% roles |
      (edges$child_type == "resource" & edges$child_url %in% resolved)])
  }

  # 4. FDP -> Catalog discovery.
  # fdp_edges contains explicit metadataCatalog links; catalog_owners includes
  # both explicit and LDP-only discoveries so unlinked catalogs are also crawled.
  fdp_list <- urls(norm(fdp_list))
  fdp_edges <- empty(fdp = character(), catalog = character())
  catalog_owners <- empty(fdp = character(), catalog_url = character())
  fdp_catalog_comparison <- empty(fdp = character(), catalog_url = character(),
                                   consistency_status = character())
  missing_catalogs <- empty(fdp = character(), catalog = character())
  for (fdp in fdp_list) {
    doc <- fetch_document(fdp, "FDP")
    if (doc$check_status != "ok") next
    subject <- root_subject(doc, fdp, metadata_catalog,
                            "https://w3id.org/fdp/fdp-o#FAIRDataPoint")
    checked <- compare_document(doc, subject, metadata_catalog)
    record_containers(checked, fdp, "FDP")
    fdp_catalog_comparison <- bind_rows(fdp_catalog_comparison, checked$comparison %>%
      transmute(fdp = .env$fdp, catalog_url = resource_url, consistency_status))
    fdp_edges <- bind_rows(fdp_edges, checked$listed %>% transmute(fdp = .env$fdp, catalog = resource_url))
    catalog_owners <- bind_rows(catalog_owners, checked$comparison %>%
                                transmute(fdp = .env$fdp, catalog_url = resource_url))
    # These catalogs exist in ldp:contains but have no metadataCatalog link.
    # Completely empty catalog containers have no child URL and are kept separately.
    missing_catalogs <- bind_rows(missing_catalogs, checked$comparison %>%
                                  filter(consistency_status == "container_without_listing") %>%
                                  transmute(fdp = .env$fdp, catalog = resource_url))
  }
  fdp_edges <- distinct(fdp_edges)
  catalog_owners <- distinct(catalog_owners)
  missing_catalogs <- distinct(missing_catalogs)
  fdp_summary <- empty(fdp = fdp_list) %>%
    left_join(fdp_edges %>% group_by(fdp) %>% summarise(
      n_catalogs = n_distinct(catalog), catalogs = paste(sort(unique(catalog)), collapse = ", "),
      .groups = "drop"), by = "fdp") %>%
    left_join(missing_catalogs %>% count(fdp, name = "n_unlinked_catalogs"), by = "fdp") %>%
    mutate(n_catalogs = coalesce(n_catalogs, 0L), catalogs = coalesce(catalogs, ""),
           n_unlinked_catalogs = coalesce(n_unlinked_catalogs, 0L))

  # 5. Catalog -> Resource discovery and consistency summaries.
  # Typed empty tables preserve the returned column structure even with no results.
  comparison_template <- compare_document(list(triples = triples_empty), character(), character())$comparison
  inventory <- bind_cols(empty(fdp = character(), catalog_url = character()), comparison_template)
  catalog_summaries <- bind_cols(empty(fdp = character(), catalog_url = character(),
                                       resource_type = character()), count_comparison(comparison_template)[0, ])
  catalog_to_dataset <- empty(catalog = character(), dataset = character())
  catalog_to_dataservice <- empty(fdp = character(), catalog_url = character(),
                                  dataservice = character())
  for (catalog in unique(catalog_owners$catalog_url)) {
    doc <- fetch_document(catalog, "Catalog")
    if (doc$check_status != "ok") next
    predicates <- c(has_part, paste0(dcat, c("dataset", "service")))
    if (!is.null(model)) predicates <- intersect(predicates, model$rules$predicate[model$rules$from == "catalog"])
    subject <- root_subject(doc, catalog, predicates, paste0(dcat, "Catalog"))
    checked <- compare_document(doc, subject, predicates)
    record_containers(checked, catalog, "Catalog")
    owners <- catalog_owners %>% filter(catalog_url == .env$catalog)
    # A catalog can belong to several FDPs. Attach each owner without refetching
    # the catalog; these provenance rows are distinct from unique-document counts.
    details <- merge(owners, checked$comparison, by = NULL)
    if (!is.null(traversal_result) && nrow(details)) {
      for (resource in unique(details$resource_url)) {
        types <- traversal_result$documents$resource_type[traversal_result$documents$url == resource]
        inferred <- if ("dataservice" %in% types) "dataservice" else
          if ("dataset" %in% types) "dataset" else NA_character_
        rows <- checked$comparison$resource_url %in% resource
        if (!is.na(inferred) && (!infer_url_types ||
            !all(tolower(checked$comparison$resource_type[rows]) %in% names(model$types)))) {
          details$resource_type[details$resource_url %in% resource] <- inferred
          checked$comparison$resource_type[rows] <- inferred
        }
      }
    }
    inventory <- bind_rows(inventory, details)
    # Identify directly linked services using the predicate, container relation,
    # local rdf:type, or familiar URL paths (not URL paths alone).
    service_containers <- checked$containers %>%
      filter(member_relation == paste0(dcat, "service")) %>% pull(container_url)
    typed_services <- doc$triples %>%
      filter(p == rdf_type, o == paste0(dcat, "DataService")) %>% pull(s)
    catalog_services <- checked$comparison %>%
      filter(listed_predicate == paste0(dcat, "service") |
               container_url %in% service_containers | resource_url %in% typed_services |
               tolower(resource_type) %in% c("dataservice", "dataservice1", "dataservice2")) %>%
      distinct(dataservice = resource_url)
    if (!is.null(model)) catalog_services <- catalog_services %>%
      filter(dataservice %in% projection_targets(catalog, "dataservice"))
    catalog_to_dataservice <- bind_rows(catalog_to_dataservice,
                                        merge(owners, catalog_services, by = NULL))
    types <- unique(checked$comparison$resource_type)
    # Retain a zero-count summary row for a successfully parsed empty catalog.
    if (!length(types)) types <- NA_character_
    for (type in types) {
      subset <- checked$comparison %>% filter(resource_type %in% .env$type)
      catalog_summaries <- bind_rows(catalog_summaries, merge(owners,
        bind_cols(empty(resource_type = type), count_comparison(subset)), by = NULL))
    }
    catalog_to_dataset <- bind_rows(catalog_to_dataset, checked$listed %>%
      filter(listed_predicate == paste0(dcat, "dataset")) %>%
      transmute(catalog = .env$catalog, dataset = resource_url))
  }
  inventory <- distinct(inventory)
  resource_urls_by_catalog <- inventory %>%
    distinct(fdp, catalog_url, resource_type, resource_url)
  # 6. Resource -> Distribution discovery.
  # Only these resource types are traversed further. Catalog DataServices are
  # excluded here and processed in the common service loop below.
  eligible <- resource_urls_by_catalog %>%
    filter(tolower(resource_type) %in% intersect(c("dataset", "patientregistry", "biobank", "guideline", "guidelines"),
      if (is.null(model)) c("dataset", "patientregistry", "biobank", "guideline", "guidelines") else names(model$types))) %>%
    anti_join(catalog_to_dataservice, by = c("fdp", "catalog_url", "resource_url" = "dataservice"))
  distribution_template <- comparison_template %>%
    select(distribution = resource_url, container_url, listed_predicate,
           listed_in_resource = listed_in_catalog, present_in_container, consistency_status)
  provenance <- eligible[0, ]
  distribution_comparison <- bind_cols(provenance, distribution_template)
  distribution_containers <- bind_cols(provenance, empty(container_url = character(),
    membership_resource = character(), member_relation = character(), n_contains = integer()))
  distribution_summary <- bind_cols(provenance, empty(check_status = character(),
    http_status = integer(), error = character(), n_listed_distributions = integer(),
    n_container_distributions = integer(), n_matched = integer(),
    n_listed_without_container = integer(), n_container_without_listing = integer(),
    all_match = logical()))
  resource_documents <- bind_cols(provenance, empty(document_url = character(),
    check_status = character(), http_status = integer(), error = character(), turtle_text = character()))
  resource_to_distribution <- bind_cols(provenance, empty(distribution = character()))
  dataset_service_comparison <- bind_cols(provenance, empty(dataservice = character(),
    container_url = character(), listed_predicate = character(), consistency_status = character()))

  for (resource in unique(eligible$resource_url)) {
    owners <- eligible %>% filter(resource_url == .env$resource)
    doc <- fetch_document(resource, paste(unique(owners$resource_type), collapse = "/"))
    resource_documents <- bind_rows(resource_documents, owners %>% mutate(
      document_url = doc$document_url, check_status = doc$check_status,
      http_status = doc$http_status, error = doc$error, turtle_text = doc$turtle_text))
    # Failure means unknown child counts (NA), not a confirmed empty resource.
    counts <- empty(n_listed_distributions = NA_integer_, n_container_distributions = NA_integer_,
                    n_matched = NA_integer_, n_listed_without_container = NA_integer_,
                    n_container_without_listing = NA_integer_, all_match = NA)
    if (doc$check_status == "ok") {
      predicates <- c(P_DCAT_DISTRIBUTION, if (!is.null(model)) P_DCAT_DATASERVICE)
      subject <- root_subject(doc, resource, predicates, paste0(dcat, "Dataset"))
      checked <- compare_document(doc, subject, predicates)
      record_containers(checked, resource, "Resource")
      if (!is.null(model)) {
        services <- checked$comparison %>% filter(resource_url %in% projection_targets(resource, "dataservice")) %>%
          transmute(dataservice = resource_url, container_url, listed_predicate, consistency_status)
        dataset_service_comparison <- bind_rows(dataset_service_comparison, merge(owners, services, by = NULL))
        checked$comparison <- checked$comparison %>% filter(resource_url %in% projection_targets(resource, "distribution"))
        checked$listed <- checked$listed %>% filter(resource_url %in% projection_targets(resource, "distribution"))
      }
      comparison <- checked$comparison %>% select(distribution = resource_url, container_url,
        listed_predicate, listed_in_resource = listed_in_catalog, present_in_container, consistency_status)
      distribution_comparison <- bind_rows(distribution_comparison, merge(owners, comparison, by = NULL))
      distribution_containers <- bind_rows(distribution_containers, merge(owners, checked$containers, by = NULL))
      # This legacy connection table keeps explicit links only; the comparison
      # table also retains LDP-only children for subsequent crawling and paths.
      resource_to_distribution <- bind_rows(resource_to_distribution, merge(owners,
        checked$listed %>% distinct(distribution = resource_url), by = NULL))
      # all_match concerns child-link agreement. Empty-container flags are added
      # separately when table2 evaluates entire paths.
      counts <- count_comparison(checked$comparison) %>%
        rename(n_listed_distributions = n_listed_resources, n_container_distributions = n_container_resources) %>%
        mutate(all_match = n_listed_without_container == 0L & n_container_without_listing == 0L)
    }
    distribution_summary <- bind_rows(distribution_summary, merge(owners, counts, by = NULL) %>%
      mutate(check_status = doc$check_status, http_status = doc$http_status, error = doc$error))
  }

  # 7. Distribution -> DataService discovery.
  # Include container-only discoveries while preserving their parent provenance.
  # accessService/contains comparisons use the DISTRIBUTION's Turtle.
  distribution_owners <- distribution_comparison %>%
    distinct(fdp, catalog_url, resource_type, resource_url, distribution)
  distribution_urls <- unique(distribution_owners$distribution)
  distribution_documents <- bind_cols(distribution_owners[0, ], empty(
    document_url = character(), check_status = character(), http_status = integer(),
    error = character(), turtle_text = character()))
  distribution_to_dataservice <- empty(distribution = character(), dataservice = character())
  distribution_service_comparison <- empty(distribution = character(), dataservice = character(),
    container_url = character(), listed_predicate = character(), consistency_status = character())
  for (distribution in distribution_urls) {
    doc <- fetch_document(distribution, "Distribution")
    distribution_documents <- bind_rows(distribution_documents,
      distribution_owners %>% filter(distribution == .env$distribution) %>% mutate(
        document_url = doc$document_url, check_status = doc$check_status,
        http_status = doc$http_status, error = doc$error, turtle_text = doc$turtle_text))
    if (doc$check_status != "ok") next
    subject <- root_subject(doc, distribution, P_DCAT_DATASERVICE, paste0(dcat, "Distribution"))
    checked <- compare_document(doc, subject, P_DCAT_DATASERVICE)
    record_containers(checked, distribution, "Distribution")
    distribution_service_comparison <- bind_rows(distribution_service_comparison,
      checked$comparison %>% transmute(distribution = .env$distribution,
        dataservice = resource_url, container_url, listed_predicate, consistency_status))
    distribution_to_dataservice <- bind_rows(distribution_to_dataservice,
      checked$comparison %>% distinct(dataservice = resource_url) %>%
        mutate(distribution = .env$distribution, .before = 1))
  }
  # 8. Combine configured service discovery routes and inspect DataService Turtle.
  # Keep source/path information for reporting, but fetch each service URL once.
  dataservice_inventory <- bind_rows(
    catalog_to_dataservice %>% mutate(source = "direct_catalog",
      resource_type = NA_character_, resource_url = NA_character_, distribution = NA_character_),
    distribution_owners %>%
      inner_join(distribution_to_dataservice, by = "distribution", relationship = "many-to-many") %>%
      mutate(source = "via_distribution"),
    dataset_service_comparison %>% mutate(source = "direct_dataset", distribution = NA_character_)) %>%
    distinct(fdp, catalog_url, source, resource_type, resource_url, distribution, dataservice)
  if (!is.null(traversal_result)) {
    service_types <- names(model$types)[vapply(model$types, function(x) "dataservice" %in% x$ancestors, logical(1))]
    extra_services <- traversal_result$documents %>%
      filter(resource_type %in% service_types, !url %in% dataservice_inventory$dataservice) %>%
      transmute(fdp, catalog_url = NA_character_, source = "configured_relationship",
        resource_type, resource_url = NA_character_, distribution = NA_character_, dataservice = url) %>% distinct()
    dataservice_inventory <- bind_rows(dataservice_inventory, extra_services)
  }
  dataservice_documents <- empty(dataservice = character(), document_url = character(),
    check_status = character(), http_status = integer(), error = character(),
    turtle_text = character(), service_reachable = logical(), has_endpointURL = logical(),
    n_endpoint_urls = integer())
  service_endpoints <- empty(dataservice = character(), endpoint_url = character())
  for (service in unique(dataservice_inventory$dataservice)) {
    doc <- fetch_document(service, "DataService")
    endpoints <- character()
    # NA means the service metadata could not be checked; FALSE means its Turtle
    # was parsed successfully and contains no endpointURL for the service subject.
    has_endpoint <- NA
    if (doc$check_status == "ok") {
      subject <- root_subject(doc, service, P_DCAT_ENDPOINT, paste0(dcat, "DataService"))
      values <- doc$triples %>%
        filter(norm(s) %in% norm(subject), p == P_DCAT_ENDPOINT) %>% pull(o)
      has_endpoint <- length(values) > 0L
      # Retain non-HTTP values too, so invalid endpoints remain visible.
      endpoints <- unique(values)
      service_endpoints <- bind_rows(service_endpoints,
        empty(dataservice = rep(service, length(endpoints)), endpoint_url = endpoints))
    }
    dataservice_documents <- bind_rows(dataservice_documents, empty(
      dataservice = service, document_url = doc$document_url, check_status = doc$check_status,
      http_status = doc$http_status, error = doc$error, turtle_text = doc$turtle_text,
      service_reachable = !is.na(doc$http_status) && doc$http_status >= 200L && doc$http_status < 300L,
      has_endpointURL = has_endpoint,
      n_endpoint_urls = if (doc$check_status == "ok") length(endpoints) else NA_integer_))
  }

  # 9. Check each unique endpoint URL once, even when several services share it.
  # Endpoint reachability is an HTTP check; endpoint responses need not be Turtle.
  # Only a final 2xx GET response passes. Timeouts, errors, non-HTTP URLs, and
  # other status codes fail; response content and API functionality are not tested.
  endpoint_checks <- empty(endpoint_url = character(), final_url = character(),
    http_status = integer(), reachable = logical(), check_status = character(), error = character())
  for (endpoint in unique(service_endpoints$endpoint_url)) {
    message("Checking endpoint: ", endpoint)
    check <- empty(endpoint_url = endpoint, final_url = endpoint, http_status = NA_integer_,
                   reachable = FALSE, check_status = "unsupported_url", error = NA_character_)
    if (grepl("^https?://", endpoint)) {
      response <- tryCatch(endpoint_fetch_fun(endpoint), error = function(e) e)
      if (inherits(response, "error")) {
        check$check_status <- "request_failed"
        check$error <- conditionMessage(response)
      } else {
        check$http_status <- as.integer(response$status_code)
        if (!is.null(response$url)) check$final_url <- response$url
        check$reachable <- !is.na(check$http_status) &&
          check$http_status >= 200L && check$http_status < 300L
        check$check_status <- if (check$reachable) "ok" else "http_error"
        if (!check$reachable) check$error <- paste("HTTP", check$http_status)
      }
    } else {
      check$error <- "Endpoint is not an HTTP(S) URL"
    }
    endpoint_checks <- bind_rows(endpoint_checks, check)
  }
  # Reattach shared endpoint results to their services. A service passes when at
  # least one endpoint is reachable; failed metadata checks retain NA counts.
  dataservice_endpoint_checks <- service_endpoints %>%
    left_join(endpoint_checks, by = "endpoint_url")
  endpoint_counts <- dataservice_endpoint_checks %>% group_by(dataservice) %>%
    summarise(n_reachable_endpoints = sum(reachable),
              n_unreachable_endpoints = sum(!reachable), .groups = "drop")
  dataservice_checks <- dataservice_documents %>% select(-turtle_text) %>%
    left_join(endpoint_counts, by = "dataservice") %>%
    mutate(n_reachable_endpoints = if_else(check_status == "ok",
             coalesce(n_reachable_endpoints, 0L), NA_integer_),
           n_unreachable_endpoints = if_else(check_status == "ok",
             coalesce(n_unreachable_endpoints, 0L), NA_integer_),
           has_reachable_endpoint = n_reachable_endpoints > 0L)
  dataservice_inventory <- dataservice_inventory %>%
    left_join(dataservice_checks, by = "dataservice")

  # 10. Service counts per catalog, per FDP, and across the run.
  # Count unique services within each grouping, not discovery-path rows.
  # The "all" rows count the union, including services discovered by both paths once.
  summary_rows <- bind_rows(dataservice_inventory,
    dataservice_inventory %>% mutate(source = "all"))
  summarise_services <- function(groups, base) {
    counts <- summary_rows %>%
      distinct(across(all_of(c(groups, "dataservice"))), .keep_all = TRUE) %>%
      group_by(across(all_of(groups))) %>% summarise(
        n_dataservices = n(),
        n_reachable_dataservices = sum(service_reachable),
        n_with_endpointURL = sum(has_endpointURL %in% TRUE),
        n_without_endpointURL = sum(has_endpointURL %in% FALSE),
        n_endpoint_unknown = sum(is.na(has_endpointURL)),
        n_with_reachable_endpoint = sum(has_reachable_endpoint %in% TRUE),
        n_with_no_reachable_endpoint = sum(has_endpointURL %in% TRUE &
                                           has_reachable_endpoint %in% FALSE),
        .groups = "drop")
    base %>% left_join(counts, by = groups) %>%
      mutate(across(starts_with("n_"), ~ coalesce(.x, 0L)))
  }
  sources <- empty(source = c("direct_catalog", if (!is.null(model)) "direct_dataset", "via_distribution", "all"))
  dataservice_summary <- summarise_services(c("fdp", "catalog_url", "source"),
    cross_join(catalog_owners, sources))
  dataservice_fdp_summary <- summarise_services(c("fdp", "source"),
    cross_join(empty(fdp = fdp_list), sources))
  dataservice_totals <- summarise_services("source", sources)
  log <- bind_rows(empty(level = character(), url = character(), document_url = character(),
    http_status = integer(), check_status = character(), error = character()), bind_rows(fetch_log))
  failures <- log %>% filter(check_status != "ok")
  # 11. Table 1: successful and failed metadata processing attempts per category.
  # Success requires fetched, parsed Turtle; failure includes fetch/parse errors.
  # Count attempted URLs only, excluding endpoint probes and undiscovered children.
  # Deduplication is per category/run; main.R places active/inactive counts side by side.
  metadata_layers <- c("FDPs", "Catalogs", "Resources - Datasets", "Resources - Biobanks",
    "Resources - Patient registries", "Resources - Guidelines", "Resources - Data Services",
    "Distributions", "DataServices")
  if (!infer_url_types) metadata_layers <- setdiff(metadata_layers,
    c("Resources - Biobanks", "Resources - Patient registries", "Resources - Guidelines"))
  # Classify by discovery path, not the first fetch's log label. A service found
  # through both paths belongs to both service rows, but is fetched only once.
  metadata_documents <- bind_rows(
    empty(`Metadata Layer` = rep("FDPs", length(fdp_list)), url = fdp_list),
    catalog_owners %>% transmute(`Metadata Layer` = "Catalogs", url = catalog_url),
    eligible %>% transmute(`Metadata Layer` = case_when(
      tolower(resource_type) == "dataset" ~ "Resources - Datasets",
      tolower(resource_type) == "biobank" ~ "Resources - Biobanks",
      tolower(resource_type) == "patientregistry" ~ "Resources - Patient registries",
      tolower(resource_type) %in% c("guideline", "guidelines") ~ "Resources - Guidelines"),
      url = resource_url),
    catalog_to_dataservice %>%
      transmute(`Metadata Layer` = "Resources - Data Services", url = dataservice),
    distribution_owners %>% transmute(`Metadata Layer` = "Distributions", url = distribution),
    distribution_to_dataservice %>% transmute(`Metadata Layer` = "DataServices", url = dataservice),
    dataset_service_comparison %>% transmute(`Metadata Layer` = "DataServices", url = dataservice)) %>%
    distinct(`Metadata Layer`, url) %>%
    inner_join(log %>% select(url, check_status), by = "url")
  processed_documents <- metadata_documents %>%
    group_by(`Metadata Layer`) %>%
    summarise(`Fetch successful` = sum(check_status == "ok"),
              `Fetch failed` = sum(check_status != "ok"), .groups = "drop")
  table1 <- empty(`Metadata Layer` = metadata_layers) %>%
    left_join(processed_documents, by = "Metadata Layer") %>%
    mutate(`Fetch successful` = coalesce(`Fetch successful`, 0L),
           `Fetch failed` = coalesce(`Fetch failed`, 0L))

  # 12. Table 2: distinct terminal URL paths. Mismatch evidence remains in path_details.
  # Collapse comparison rows to edges before expanding paths: repeated predicates
  # or multiple containers must not multiply the same sequence of URLs.
  catalog_path_edges <- fdp_catalog_comparison %>% group_by(fdp, catalog_url) %>%
    summarise(fdp_catalog_mismatch = any(consistency_status != "matched"), .groups = "drop")
  resource_path_edges <- inventory %>% group_by(fdp, catalog_url, resource_url, resource_type) %>%
    summarise(catalog_resource_mismatch = any(consistency_status != "matched"), .groups = "drop") %>%
    left_join(catalog_to_dataservice %>% distinct(fdp, catalog_url, resource_url = dataservice) %>%
      mutate(is_service = TRUE), by = c("fdp", "catalog_url", "resource_url")) %>%
    mutate(resource_layer = case_when(
      is_service %in% TRUE ~ "DataService",
      tolower(resource_type) == "dataset" ~ "Dataset",
      tolower(resource_type) == "biobank" ~ "Biobank",
      tolower(resource_type) %in% c("guideline", "guidelines") ~ "Guideline",
      tolower(resource_type) == "patientregistry" ~ "PatientRegistry",
      TRUE ~ resource_type))
  distribution_path_edges <- distribution_comparison %>%
    group_by(fdp, catalog_url, resource_url, distribution) %>%
    summarise(resource_distribution_mismatch = any(consistency_status != "matched"), .groups = "drop")
  service_path_edges <- distribution_service_comparison %>% group_by(distribution, dataservice) %>%
    summarise(distribution_service_mismatch = any(consistency_status != "matched"), .groups = "drop")
  # Union the direct and distribution routes before joining their parents.
  # Joining direct services onto distribution rows would invent cross-products.
  resource_branches <- distribution_path_edges %>%
    left_join(service_path_edges, by = "distribution", relationship = "many-to-many", na_matches = "never")
  direct_branches <- dataset_service_comparison %>% group_by(fdp, catalog_url, resource_url, dataservice) %>%
    summarise(dataset_service_mismatch = any(consistency_status != "matched"), .groups = "drop") %>%
    mutate(distribution = NA_character_)
  resource_branches <- bind_rows(resource_branches, direct_branches)
  document_status <- log %>% distinct(url, check_status)
  empty_containers <- container_checks %>% filter(n_contains == 0L)
  empty_container_parents <- function(parent_level, url_column, flag_column) {
    empty_containers %>% filter(level == .env$parent_level) %>% distinct(parent_url) %>%
      transmute(!!url_column := parent_url, !!flag_column := TRUE)
  }
  # Left joins preserve parents with no children. Joining on FDP/catalog/resource
  # provenance keeps shared children associated with their actual parent paths.
  path_details <- empty(fdp = fdp_list) %>%
    left_join(catalog_path_edges, by = "fdp") %>%
    left_join(resource_path_edges, by = c("fdp", "catalog_url"), relationship = "many-to-many") %>%
    left_join(resource_branches, by = c("fdp", "catalog_url", "resource_url"),
              relationship = "many-to-many") %>%
    left_join(document_status %>% rename(fdp = url, fdp_status = check_status), by = "fdp") %>%
    left_join(document_status %>% rename(catalog_url = url, catalog_status = check_status), by = "catalog_url") %>%
    left_join(document_status %>% rename(resource_url = url, resource_status = check_status), by = "resource_url") %>%
    left_join(document_status %>% rename(distribution = url, distribution_status = check_status), by = "distribution") %>%
    left_join(empty_container_parents("FDP", "fdp", "fdp_empty_container"), by = "fdp") %>%
    left_join(empty_container_parents("Catalog", "catalog_url", "catalog_empty_container"), by = "catalog_url") %>%
    left_join(empty_container_parents("Resource", "resource_url", "resource_empty_container"), by = "resource_url") %>%
    left_join(empty_container_parents("Distribution", "distribution", "distribution_empty_container"), by = "distribution") %>%
    mutate(across(ends_with("_empty_container"), ~ coalesce(.x, FALSE))) %>%
    mutate(
      # Label the deepest discovered layer, not every prefix of a longer path.
      actual_path = case_when(
        is.na(catalog_url) ~ "FDP",
        is.na(resource_url) ~ "FDP-Catalog",
        is.na(distribution) & !is.na(dataservice) ~ paste0("FDP-Catalog-", resource_layer, "-DataService"),
        is.na(distribution) ~ paste0("FDP-Catalog-", resource_layer),
        is.na(dataservice) ~ paste0("FDP-Catalog-", resource_layer, "-Distribution"),
        TRUE ~ paste0("FDP-Catalog-", resource_layer, "-Distribution-DataService")),
      # DataService is the terminal layer in this traversal, even when its own
      # Turtle fails. Earlier failures cannot establish absence of children.
      path_status = case_when(
        fdp_status != "ok" ~ fdp_status,
        !is.na(catalog_url) & catalog_status != "ok" ~ catalog_status,
        is.na(resource_url) | resource_layer == "DataService" ~ "complete",
        is.na(resource_status) ~ "not_traversed",
        resource_status != "ok" ~ resource_status,
        !is.na(distribution) & distribution_status != "ok" ~ distribution_status,
        TRUE ~ "complete"),
      dataservice = if_else(resource_layer %in% "DataService", resource_url, dataservice),
      has_link_mismatch = coalesce(fdp_catalog_mismatch, FALSE) |
        coalesce(catalog_resource_mismatch, FALSE) |
        coalesce(resource_distribution_mismatch, FALSE) |
        coalesce(dataset_service_mismatch, FALSE) |
        coalesce(distribution_service_mismatch, FALSE),
      # Empty containers have no child edge to compare. Flag their parent on
      # every terminal path through it, without inventing a child URL/path.
      has_empty_container = fdp_empty_container | catalog_empty_container |
        resource_empty_container | distribution_empty_container,
      has_mismatch = has_link_mismatch | has_empty_container) %>% distinct()
  path_labels <- c("FDP", "FDP-Catalog", "FDP-Catalog-Dataset",
    if (!is.null(model)) "FDP-Catalog-Dataset-DataService",
    "FDP-Catalog-Dataset-Distribution", "FDP-Catalog-Dataset-Distribution-DataService",
    "FDP-Catalog-Biobank", "FDP-Catalog-Guideline", "FDP-Catalog-PatientRegistry",
    "FDP-Catalog-DataService", "Another")
  if (!infer_url_types) path_labels <- path_labels[!grepl("Biobank|Guideline|PatientRegistry", path_labels)]
  path_details <- path_details %>%
    mutate(Path = if_else(actual_path %in% path_labels, actual_path, "Another"))
  # A classification fetch alone must not certify that a resource was expanded.
  if (!is.null(traversal_result)) {
    unresolved <- traversal_result$documents %>% filter(traversal_status == "unsupported_type") %>% pull(url)
    path_details$path_status[path_details$resource_url %in% unresolved] <- "not_traversed"
    projected_edges <- bind_rows(
      fdp_catalog_comparison %>% transmute(parent_url = fdp, child_url = catalog_url),
      inventory %>% transmute(parent_url = catalog_url, child_url = resource_url),
      distribution_comparison %>% transmute(parent_url = resource_url, child_url = distribution),
      distribution_service_comparison %>% transmute(parent_url = distribution, child_url = dataservice),
      dataset_service_comparison %>% transmute(parent_url = resource_url, child_url = dataservice),
      catalog_to_dataservice %>% transmute(parent_url = catalog_url, child_url = dataservice)) %>% distinct()
    outside <- traversal_result$edges %>% anti_join(projected_edges, by = c("parent_url", "child_url"))
    path_details$projection_boundary <- rep(NA_character_, nrow(path_details))
    stages <- c("fdp", "catalog_url", "resource_url", "distribution", "dataservice")
    for (stage in seq_along(stages)) {
      column <- stages[[stage]]
      stopped <- if (stage == length(stages)) rep(TRUE, nrow(path_details)) else
        is.na(path_details[[stages[[stage + 1L]]]])
      mark <- stopped & is.na(path_details$projection_boundary) & path_details[[column]] %in% outside$parent_url
      path_details$projection_boundary[mark] <- column
    }
    path_details$path_status[path_details$path_status == "complete" &
      !is.na(path_details$projection_boundary)] <- "outside_standard_route"
  }
  # Exclude failed/untraversed branches from confirmed terminal counts.
  # Longer/unlisted path shapes fall under Another.
  path_counts <- path_details %>% filter(path_status == "complete") %>% group_by(Path) %>%
    summarise(`N of times found` = n(), .groups = "drop")
  table2 <- empty(Path = path_labels) %>% left_join(path_counts, by = "Path") %>%
    mutate(`N of times found` = coalesce(`N of times found`, 0L))

  # Table 3 describes outgoing structure in each PARENT metadata layer.
  # Units: distinct parent-child links (cases 1/4), parent-container pairs (2),
  # and parent documents (3). Cases can overlap and should not be summed.
  case_names <- c("LDP-only links", "Empty LDP containers", "No DCAT or LDP continuation",
                  "Next-step fetch failed")
  parent_layers <- metadata_documents %>% transmute(`Metadata Layer`, parent_url = url,
    level = case_when(
      `Metadata Layer` == "FDPs" ~ "FDP",
      `Metadata Layer` == "Catalogs" ~ "Catalog",
      `Metadata Layer` == "Distributions" ~ "Distribution",
      `Metadata Layer` %in% c("Resources - Data Services", "DataServices") ~ "DataService",
      TRUE ~ "Resource")) %>% distinct()
  outgoing_links <- bind_rows(
    fdp_catalog_comparison %>% transmute(parent_url = fdp, child_url = catalog_url,
      level = "FDP", consistency_status),
    inventory %>% transmute(parent_url = catalog_url, child_url = resource_url,
      level = "Catalog", consistency_status),
    distribution_comparison %>% transmute(parent_url = resource_url, child_url = distribution,
      level = "Resource", consistency_status),
    dataset_service_comparison %>% transmute(parent_url = resource_url, child_url = dataservice,
      level = "Resource", consistency_status),
    distribution_service_comparison %>% transmute(parent_url = distribution, child_url = dataservice,
      level = "Distribution", consistency_status)) %>% distinct()
  metadata_case_details <- bind_rows(
    outgoing_links %>% filter(consistency_status == "container_without_listing") %>%
      transmute(parent_url, level, case = case_names[1], child_url, container_url = NA_character_,
        check_status = NA_character_, error = NA_character_),
    empty_containers %>% distinct(parent_url, level, container_url) %>%
      mutate(case = case_names[2], child_url = NA_character_, check_status = NA_character_, error = NA_character_),
    continuation_checks %>% filter(!has_metadata_links, n_containers == 0L,
      !parent_url %in% additional_relationships$parent_url) %>%
      transmute(parent_url, level, case = case_names[3], child_url = NA_character_,
        container_url = NA_character_, check_status = "ok", error = NA_character_),
    outgoing_links %>% distinct(parent_url, level, child_url) %>%
      inner_join(log %>% filter(check_status != "ok") %>%
        select(child_url = url, check_status, error), by = "child_url") %>%
      mutate(case = case_names[4], container_url = NA_character_)) %>%
    inner_join(parent_layers, by = c("parent_url", "level"), relationship = "many-to-many") %>%
    select(`Metadata Layer`, case, parent_url, child_url, container_url, check_status, error) %>% distinct()
  table3 <- empty(`Metadata Layer` = metadata_layers)
  for (case_name in case_names) {
    counts <- metadata_case_details %>% filter(case == .env$case_name) %>%
      count(`Metadata Layer`, name = case_name)
    table3 <- table3 %>% left_join(counts, by = "Metadata Layer") %>%
      mutate(!!case_name := coalesce(.data[[case_name]], 0L))
  }
  # Services are terminal here; endpointURL HTTP checks are not another DCAT/LDP layer.
  table3 <- table3 %>% mutate(across(all_of(case_names),
    ~ if_else(`Metadata Layer` %in% c("Resources - Data Services", "DataServices"), NA_integer_, .x)))

  if (!is.null(traversal_result)) {
    # Queue evidence is authoritative for all configured types. In particular,
    # projection-only comparisons must not invent dead ends on custom routes.
    additional_relationships <- traversal_result$additional
    metadata_case_details <- traversal_result$cases %>% select(-fdp)
    failed_children <- traversal_result$edges %>%
      inner_join(log %>% filter(check_status != "ok") %>%
        select(child_url = url, check_status, error), by = "child_url") %>%
      transmute(`Metadata Layer` = parent_type, case = "Next-step fetch failed",
        parent_url, child_url, container_url, check_status, error)
    metadata_case_details <- bind_rows(metadata_case_details, failed_children) %>% distinct()
    # Keep the compatibility table shape, but derive its rows from queue cases.
    table3 <- metadata_case_details %>% count(`Metadata Layer`, case) %>%
      tidyr::pivot_wider(names_from = case, values_from = n, values_fill = 0L)
    for (case_name in case_names) if (!case_name %in% names(table3)) table3[[case_name]] <- rep(0L, nrow(table3))
  }

  # 13. Prepare URL lists, legacy connection tables, and the graph export.
  # Discovered URL lists may include failed documents; table1 reports both outcomes.
  # The merged graph is not used to infer per-catalog containment.
  get_type_urls <- function(types) resource_urls_by_catalog %>%
    filter(tolower(resource_type) %in% types) %>% pull(resource_url) %>% unique()
  dataset_to_distribution <- resource_to_distribution %>%
    filter(tolower(resource_type) == "dataset") %>%
    distinct(dataset = resource_url, distribution)
  df_triples <- rdflib::rdf_query(graph, "SELECT ?s ?p ?o WHERE { ?s ?p ?o }")
  if (nrow(df_triples) == 0L) df_triples <- triples_empty

  # Return values are accessed by the caller, e.g. result_active$table1.
  list(
    # Presentation tables and the detailed evidence behind terminal-path counts.
    table1 = table1,
    traversal_documents = if (!is.null(traversal_result)) traversal_result$documents else NULL,
    traversal_edges = if (!is.null(traversal_result)) traversal_result$edges else NULL,
    additional_relationships = additional_relationships,
    table2 = table2,
    table3 = table3,
    metadata_case_details = metadata_case_details,
    continuation_checks = continuation_checks,
    path_details = path_details,
    incomplete_paths = path_details %>% filter(path_status != "complete"),
    container_checks = container_checks,
    empty_containers = empty_containers,
    fdp_catalog_comparison = fdp_catalog_comparison,
    distribution_service_comparison = distribution_service_comparison,
    dataset_service_comparison = dataset_service_comparison,
    # Combined RDF plus connections used by the existing layer/Sankey functions.
    graph = graph, df_triples = as_tibble(df_triples),
    connections = list(fdp_to_catalog = fdp_edges, catalog_to_dataset = distinct(catalog_to_dataset),
      dataset_to_distribution = dataset_to_distribution,
      dataset_to_dataservice = dataset_service_comparison %>% distinct(dataset = resource_url, dataservice),
      distribution_to_dataservice = distinct(distribution_to_dataservice),
      catalog_to_dataservice = distinct(catalog_to_dataservice),
      resource_to_distribution = resource_to_distribution),
    # Catalog/resource inventory and child-link problems (empty containers above).
    fdp_summary = fdp_summary, catalogs_not_linked_to_FDP = missing_catalogs,
    all_resource_type_summary_df = catalog_summaries,
    all_resource_inventory_df = inventory,
    all_problem_resources_df = inventory %>% filter(consistency_status != "matched"),
    resource_urls_by_catalog = resource_urls_by_catalog,
    dataset_urls = get_type_urls("dataset"), patientregistry_urls = get_type_urls("patientregistry"),
    biobank_urls = get_type_urls("biobank"),
    dataservice_urls = unique(dataservice_inventory$dataservice),
    catalog_dataservice_urls = unique(catalog_to_dataservice$dataservice),
    distribution_dataservice_urls = unique(distribution_to_dataservice$dataservice),
    distribution_urls = distribution_urls,
    # Downloaded Turtle, fetch/parse outcomes, service endpoints, and service counts.
    distribution_documents = distribution_documents,
    dataservice_inventory = dataservice_inventory,
    dataservice_documents = dataservice_documents,
    dataservice_checks = dataservice_checks,
    endpoint_checks = endpoint_checks,
    dataservice_endpoint_checks = dataservice_endpoint_checks,
    dataservice_summary = dataservice_summary,
    dataservice_fdp_summary = dataservice_fdp_summary,
    dataservice_totals = dataservice_totals,
    # Resource -> Distribution comparison, including LDP-only discoveries.
    resource_documents = resource_documents,
    distribution_container_comparison = distribution_comparison,
    distribution_containers = distribution_containers,
    distribution_summary = distribution_summary,
    problem_distributions = distribution_comparison %>% filter(consistency_status != "matched"),
    # One metadata log row per attempted URL; endpoint checks have their own table.
    fetch_log = log, failures = failures
  )
}
