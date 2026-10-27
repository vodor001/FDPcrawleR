# Presentation only: reconstruct the original layer/path/case aggregations from
# saved evidence, never from the first-fetch log role and never by recrawling.
dashboard_layer <- function(x) {
  raw <- sub("^Resources -\\s*", "", x)
  key <- tolower(gsub("[ _-]", "", x))
  map <- c(fdp = "FDPs", fdps = "FDPs", catalog = "Catalogs", catalogs = "Catalogs",
    dataset = "Dataset", datasets = "Dataset", resourcesdatasets = "Dataset",
    resourcesdataservices = "DataServices2", resourcesdataservice = "DataServices2",
    dataservice = "DataServices1", dataservices = "DataServices1",
    dataservice1 = "DataServices1", dataservices1 = "DataServices1", dataservices2 = "DataServices2",
    distribution = "Distributions", distributions = "Distributions",
    biobank = "Biobanks", biobanks = "Biobanks", resourcesbiobanks = "Biobanks",
    patientregistry = "Patient registries", patientregistries = "Patient registries",
    resourcespatientregistries = "Patient registries", guideline = "Guidelines",
    guidelines = "Guidelines", resourcesguidelines = "Guidelines")
  found <- unname(map[key])
  ifelse(is.na(found), raw, found)
}
dashboard_state_label <- function(state) {
  known <- c(ACTIVE = "Active", INACTIVE = "Inactive", INVALID = "Invalid",
    UNKNOWN = "Unknown", UNREACHABLE = "Unreachable")
  label <- unname(known[state])
  paste(ifelse(is.na(label), state, label), "FDP")
}

# Public dashboard labels and column layout. The normalized labels
# above are internal keys only; do not change the resource evidence itself.
dashboard_display_layer <- function(x) {
  if (!length(x)) return(character())
  labels <- c(Dataset = "Resources - Datasets", Biobanks = "Resources - Biobanks",
    `Patient registries` = "Resources - Patient registries", Guidelines = "Resources - Guidelines",
    DataServices2 = "Resources - Data Services", DataServices1 = "DataServices")
  mapped <- unname(labels[x])
  ifelse(is.na(mapped), x, mapped)
}

dashboard_display_summary_one <- function(pack, number, states) {
  frame <- dashboard_summary_one(pack, number, states)
  if (number != 2L) frame$`Metadata Layer` <- dashboard_display_layer(frame$`Metadata Layer`)
  if (number == 1L) {
    # Match the original check_status != "ok" calculation, including saved
    # crawler errors. Limit details remain available in the original fetch log.
    frame$`Fetch failed` <- frame$`Fetch failed` + frame$`Not requested: limit`
    frame$`Not requested: limit` <- NULL
  }
  frame
}

dashboard_display_summary <- function(pack, number, state = "ALL") {
  states <- shared_states(pack, state)
  if (length(states) == 1L) return(dashboard_display_summary_one(pack, number, states))
  key <- if (number == 2L) "Path" else "Metadata Layer"
  answer <- setNames(data.frame(character(), stringsAsFactors = FALSE), key)
  for (st in states) {
    frame <- dashboard_display_summary_one(pack, number, st)
    metrics <- setdiff(names(frame), key)
    names(frame)[match(metrics, names(frame))] <- if (number == 2L) dashboard_state_label(st) else
      paste(dashboard_state_label(st), metrics, sep = " - ")
    answer <- dplyr::full_join(answer, frame, by = key)
  }
  answer
}

dashboard_display_table4 <- function(pack, state = "ALL") {
  frame <- shared_table4(pack, state)
  # Coverage is evidence about the crawl, not an extra structural warning category.
  frame$`Warning type` <- vapply(frame$`Warning type`, function(value) {
    warnings <- setdiff(strsplit(value, "; ", fixed = TRUE)[[1]],
      c("additional relationships not traversed", "resource types not traversed"))
    if (!length(warnings)) "no warnings" else paste(warnings, collapse = "; ")
  }, character(1))
  labels <- shared_test_labels(pack)
  frame[c("FDP", if (length(labels)) paste(labels, "outcome"),
    "Warning type", "Number of FDPCrawleR warnings")]
}
dashboard_crawls <- function(pack, states) pack$crawls[intersect(shared_urls(pack, states), names(pack$crawls))]

dashboard_roles <- function(crawl, config = list()) {
  roles <- tibble::tibble(`Metadata Layer` = character(), url = character())
  add <- function(urls, labels) {
    if (!length(urls)) return(invisible(NULL))
    roles <<- dplyr::bind_rows(roles, tibble::tibble(url = urls, `Metadata Layer` = rep_len(labels, length(urls))))
  }
  add(crawl$fdp_summary$fdp, "FDPs")
  add(crawl$fdp_catalog_comparison$catalog_url, "Catalogs")
  resources <- crawl$resource_documents
  generic <- identical(config$profile, "generic")
  if (generic && !is.null(resources) && nrow(resources)) {
    # Old generic snapshots also traversed ERDERA URL categories. Only retained
    # DCAT evidence can justify displaying those documents as Dataset now.
    triples <- crawl$df_triples
    typed <- if (!is.null(triples)) triples$s[triples$p == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" &
      triples$o == "http://www.w3.org/ns/dcat#Dataset"] else character()
    norm <- function(x) sub("/+$", "", x)
    dataset <- tolower(resources$resource_type) == "dataset" |
      norm(resources$resource_url) %in% norm(typed) | norm(resources$document_url) %in% norm(typed)
    add(resources$resource_url[dataset], "Dataset")
  } else add(resources$resource_url, dashboard_layer(resources$resource_type))
  services <- crawl$dataservice_inventory
  if (!is.null(services)) {
    if (generic) services <- services[services$resource_url %in% roles$url[roles$`Metadata Layer` == "Dataset"] |
      services$source == "direct_catalog", , drop = FALSE]
    add(services$dataservice[services$source == "direct_catalog"], "DataServices2")
    add(services$dataservice[services$source != "direct_catalog"], "DataServices1")
  }
  distributions <- crawl$distribution_documents
  if (generic && !is.null(distributions)) distributions <- distributions[
    distributions$resource_url %in% roles$url[roles$`Metadata Layer` == "Dataset"], , drop = FALSE]
  add(distributions$distribution, "Distributions")
  # Configured custom routes may extend beyond the legacy projection. Preserve
  # those roles without relabelling every classification request as a Dataset.
  queue <- crawl$traversal_documents
  if (!generic && !is.null(queue) && nrow(queue)) {
    for (type in unique(queue$resource_type)) {
      rows <- queue[queue$resource_type == type & !queue$url %in% roles$url, , drop = FALSE]
      label <- config$traversal$resource_types[[type]]$label
      if (is.null(label)) label <- if (type == "resource") "Unclassified resource" else dashboard_layer(type)
      if (generic && !type %in% c("fdp", "catalog", "dataset", "distribution", "dataservice")) next
      add(rows$url, label)
    }
  }
  # Old generic snapshots contain classification fetches that never resolved.
  # Keep these attempts visible, separately from the known metadata layers.
  log <- crawl$fetch_log
  if (!generic && !is.null(log)) add(log$url[!log$url %in% roles$url], "Unclassified resource")
  dplyr::distinct(roles[!is.na(roles$url) & nzchar(roles$url), ])
}

dashboard_documents <- function(crawls, config) {
  template <- tibble::tibble(`Metadata Layer` = character(), url = character(),
    check_status = character(), error = character())
  frames <- lapply(crawls, function(crawl) {
    log <- crawl$fetch_log
    if (is.null(log) || !nrow(log)) return(template)
    if (!"error" %in% names(log)) log$error <- NA_character_
    dplyr::inner_join(dashboard_roles(crawl, config), log[c("url", "check_status", "error")], by = "url")
  })
  dplyr::bind_rows(c(list(template), unname(frames))) %>% dplyr::distinct(`Metadata Layer`, url, .keep_all = TRUE)
}

dashboard_cases <- function(crawls, config) {
  template <- tibble::tibble(`Metadata Layer` = character(), case = character(),
    parent_url = character(), child_url = character(), container_url = character())
  frames <- lapply(crawls, function(crawl) {
    frame <- crawl$metadata_case_details
    if (is.null(frame) || !nrow(frame)) return(template)
    for (key in setdiff(names(template), names(frame))) frame[[key]] <- NA_character_
    frame$`Metadata Layer` <- dashboard_layer(frame$`Metadata Layer`)
    if (identical(config$profile, "generic")) {
      roles <- dashboard_roles(crawl, config)
      frame <- dplyr::inner_join(frame %>% dplyr::select(-`Metadata Layer`),
        roles %>% dplyr::rename(parent_url = url), by = "parent_url", relationship = "many-to-many")
    }
    # Queue cases use type labels; service discovery route remains a separate
    # category. Other labels retain their original per-layer identity.
    frame[names(template)]
  })
  dplyr::distinct(dplyr::bind_rows(c(list(template), unname(frames))))
}

dashboard_paths <- function(crawls, config) {
  dplyr::bind_rows(lapply(crawls, function(crawl) {
    paths <- crawl$path_details
    if (is.null(paths) || !nrow(paths) || !identical(config$profile, "generic")) return(paths)
    roles <- dashboard_roles(crawl, config)
    datasets <- roles$url[roles$`Metadata Layer` == "Dataset"]
    known <- paths$resource_url %in% datasets
    services <- paths$resource_url %in% roles$url[roles$`Metadata Layer` == "DataServices2"]
    paths$resource_layer[known] <- "Dataset"
    paths$resource_layer[services] <- "DataService"
    outside <- !is.na(paths$resource_url) & !known & !services
    paths$resource_layer[outside] <- "unknown"
    paths$path_status[outside & paths$path_status == "complete"] <- "outside_profile"
    paths$Path <- ifelse(is.na(paths$catalog_url), "FDP", ifelse(is.na(paths$resource_url), "FDP-Catalog",
      ifelse(outside, "Another", ifelse(!is.na(paths$distribution),
        ifelse(!is.na(paths$dataservice), "FDP-Catalog-Dataset-Distribution-DataService", "FDP-Catalog-Dataset-Distribution"),
        ifelse(!is.na(paths$dataservice), "FDP-Catalog-Dataset-DataService", "FDP-Catalog-Dataset")))))
    paths$Path[services] <- "FDP-Catalog-DataService"
    paths
  }))
}

dashboard_summary_one <- function(pack, number, states) {
  crawls <- dashboard_crawls(pack, states)
  key <- if (number == 2L) "Path" else "Metadata Layer"
  metrics <- switch(as.character(number), `1` = c("Fetch successful", "Fetch failed", "Not requested: limit"),
    `2` = "N of times found", `3` = c("LDP-only links", "Empty LDP containers", "No DCAT or LDP continuation", "Next-step fetch failed"))
  answer <- setNames(data.frame(character(), stringsAsFactors = FALSE), key)
  for (metric in metrics) answer[[metric]] <- integer()
  if (!length(crawls)) return(answer)
  if (number == 1L) {
    docs <- dashboard_documents(crawls, pack$config)
    counts <- docs %>% dplyr::group_by(`Metadata Layer`) %>% dplyr::summarise(
      `Fetch successful` = sum(check_status == "ok", na.rm = TRUE),
      `Fetch failed` = sum(!is.na(check_status) & check_status != "ok" & !grepl("Document limit reached", error, fixed = TRUE)),
      `Not requested: limit` = sum(grepl("Document limit reached", error, fixed = TRUE)), .groups = "drop")
    layers <- unique(c(unlist(lapply(crawls, function(x) dashboard_layer(x$table1$`Metadata Layer`))), docs$`Metadata Layer`))
  } else if (number == 2L) {
    paths <- dashboard_paths(crawls, pack$config)
    if (!nrow(paths)) return(answer)
    paths <- paths %>% dplyr::distinct(fdp, catalog_url, resource_url, distribution, dataservice, .keep_all = TRUE)
    # The old Table 2 counts confirmed terminal paths, not HTTP successes.
    counts <- paths %>% dplyr::filter(path_status == "complete") %>% dplyr::count(Path, name = "N of times found")
    layers <- unique(c(unlist(lapply(crawls, function(x) x$table2$Path)), paths$Path))
  } else {
    cases <- dashboard_cases(crawls, pack$config)
    layers <- unique(c(unlist(lapply(crawls, function(x) dashboard_layer(x$table1$`Metadata Layer`))), cases$`Metadata Layer`))
    counts <- cases %>% dplyr::filter(case %in% metrics) %>% dplyr::count(`Metadata Layer`, case) %>%
      tidyr::pivot_wider(names_from = case, values_from = n, values_fill = 0L)
  }
  layers <- layers[!is.na(layers)]
  if (identical(pack$config$profile, "generic")) layers <- if (number == 2L)
    intersect(layers, c("FDP", "FDP-Catalog", "FDP-Catalog-Dataset", "FDP-Catalog-Dataset-Distribution",
      "FDP-Catalog-Dataset-DataService", "FDP-Catalog-Dataset-Distribution-DataService", "FDP-Catalog-DataService")) else
    intersect(layers, c("FDPs", "Catalogs", "Dataset", "DataServices2", "Distributions", "DataServices1"))
  answer <- setNames(data.frame(layers, stringsAsFactors = FALSE), key)
  answer <- dplyr::left_join(answer, counts, by = key)
  for (metric in metrics) {
    if (!metric %in% names(answer)) answer[[metric]] <- 0L
    answer[[metric]][is.na(answer[[metric]])] <- 0L
    if (number == 3L) answer[[metric]][answer[[key]] %in% c("DataServices1", "DataServices2")] <- NA_integer_
  }
  answer[c(key, metrics)]
}

dashboard_summary <- function(pack, number, state = "ALL") {
  states <- shared_states(pack, state)
  key <- if (number == 2L) "Path" else "Metadata Layer"
  answer <- setNames(data.frame(character(), stringsAsFactors = FALSE), key)
  for (st in states) {
    frame <- dashboard_summary_one(pack, number, st)
    metrics <- setdiff(names(frame), key)
    names(frame)[match(metrics, names(frame))] <- if (number == 2L) dashboard_state_label(st) else
      paste(dashboard_state_label(st), metrics, sep = " - ")
    answer <- dplyr::full_join(answer, frame, by = key)
  }
  if (length(states) > 1L) {
    total <- dashboard_summary_one(pack, number, states)
    metrics <- setdiff(names(total), key)
    names(total)[match(metrics, names(total))] <- if (number == 2L) "Total" else paste("Total", metrics, sep = " - ")
    answer <- dplyr::left_join(answer, total, by = key)
  }
  # Retain the original columns unless document limits actually skipped work.
  limit_columns <- grep("Not requested: limit$", names(answer), value = TRUE)
  if (length(limit_columns) && !any(unlist(answer[limit_columns]) > 0L, na.rm = TRUE)) answer[limit_columns] <- NULL
  answer
}

dashboard_warning_rows <- function(pack, state = "ALL") {
  totals <- shared_table4(pack, state)
  totals$`Total FDPCrawleR warnings` <- totals$`Number of FDPCrawleR warnings`
  totals$`Coverage notice` <- rep("", nrow(totals))
  if (!nrow(totals)) return(totals)
  dplyr::bind_rows(lapply(seq_len(nrow(totals)), function(i) {
    row <- totals[i, , drop = FALSE]
    values <- strsplit(row$`Warning type`, "; ", fixed = TRUE)[[1]]
    coverage <- values[values %in% c("additional relationships not traversed", "resource types not traversed")]
    row$`Coverage notice` <- paste(coverage, collapse = "; ")
    values <- setdiff(values, coverage)
    if (!length(values)) values <- "no warnings"
    dplyr::bind_rows(lapply(values, function(warning) {
      detail <- row
      detail$`Warning type` <- warning
      if (warning %in% unname(fdp_warning_labels)) {
        crawl <- pack$crawls[[row$FDP]]
        if (identical(pack$config$profile, "generic")) crawl$metadata_case_details <- dashboard_cases(list(crawl), pack$config)
        # Reuse the original occurrence counter with only this warning category.
        crawl$metadata_case_details <- crawl$metadata_case_details[
          crawl$metadata_case_details$case %in% names(fdp_warning_labels)[fdp_warning_labels == warning], , drop = FALSE]
        detail$`Number of FDPCrawleR warnings` <- if (is.na(row$`Total FDPCrawleR warnings`)) NA_integer_ else
          fdp_crawler_warning_counts(row$FDP, list(crawl), warning)
      }
      detail
    }))
  }))
}

dashboard_overview <- function(pack, state = "ALL") {
  index <- shared_index(pack, state)
  states <- shared_states(pack, state)
  valid <- !is.na(index$valid_url) & index$valid_url & !is.na(index$FDP) & nzchar(index$FDP)
  totals <- shared_table4(pack, state)
  list(fdps = length(unique(index$FDP[valid])), entries = nrow(index), invalid_urls = sum(!valid),
    states = setNames(vapply(states, function(st) length(unique(index$FDP[valid & index$State %in% st])), integer(1)), states),
    state_entries = setNames(vapply(states, function(st) sum(index$State %in% st), integer(1)), states),
    warnings = sum(totals$`Number of FDPCrawleR warnings` > 0L, na.rm = TRUE))
}
