shared_index <- function(pack, state = "ALL") {
  index <- pack$index
  index$State[is.na(index$State) | !nzchar(index$State)] <- "UNKNOWN"
  index$State[index$State == "UNREACHEABLE"] <- "UNREACHABLE"
  if (!"ALL" %in% state) index <- index[index$State %in% state, , drop = FALSE]
  index
}
shared_states <- function(pack, state = "ALL") {
  available <- unique(shared_index(pack)$State)
  ordered <- unique(c(intersect(c("ACTIVE", "INACTIVE", "INVALID", "UNKNOWN", "UNREACHABLE"), available), available))
  if ("ALL" %in% state) ordered else intersect(ordered, state)
}
shared_urls <- function(pack, state = "ALL") {
  index <- shared_index(pack, state)
  unique(index$FDP[!is.na(index$valid_url) & index$valid_url & !is.na(index$FDP) & nzchar(index$FDP)])
}
shared_assessments <- function(pack, state = "ALL") {
  urls <- intersect(shared_urls(pack, state), names(pack$assessments))
  dplyr::bind_rows(lapply(pack$assessments[urls], function(x) dplyr::bind_rows(x)))
}
shared_test_labels <- function(pack) {
  tests <- pack$config$assessments
  if (!length(tests)) return(setNames(character(), character()))
  # Old version-2 snapshots stored assessments: [erdera].
  if (is.null(names(tests))) return(c(l1 = "L1", l2 = "L2"))
  vapply(tests, function(test) test$label, character(1))
}
shared_table4 <- function(pack, state = "ALL") {
  index <- shared_index(pack, state)
  urls <- unique(index$FDP)
  if (!length(urls)) {
    empty <- data.frame(FDP = character(), `Index state` = character(),
    `Crawl status` = character(), `Warning type` = character(), `Number of FDPCrawleR warnings` = integer(),
    `Traversal completeness` = character(), check.names = FALSE)
    for (label in shared_test_labels(pack)) empty[[paste(label, "outcome")]] <- character()
    return(empty)
  }
  rows <- lapply(urls, function(url) {
    membership <- index[index$FDP %in% url, , drop = FALSE]
    crawl <- if (!is.na(url) && nzchar(url)) pack$crawls[[url]] else NULL
    status <- warning <- "not selected"
    count <- NA_integer_
    completeness <- "not assessed"
    if (!all(membership$valid_url)) status <- warning <- "invalid/missing URL" else if (!is.null(crawl)) {
      if (identical(pack$config$profile, "generic")) crawl$metadata_case_details <- dashboard_cases(list(crawl), pack$config)
      root <- crawl$fetch_log[crawl$fetch_log$url %in% url, , drop = FALSE]
      status <- if (nrow(root)) root$check_status[[1]] else "not assessed"
      warning <- fdp_crawler_warnings(url, crawl)
      count <- fdp_crawler_warning_counts(url, list(crawl), warning)
      queue <- crawl$traversal_documents
      partial <- any(crawl$fetch_log$check_status != "ok") ||
        (if (is.null(queue)) any(crawl$path_details$path_status != "complete") else
          any(queue$traversal_status != "expanded")) || nrow(crawl$additional_relationships) > 0L
      completeness <- if (partial) "incomplete" else "complete within supported relationships"
      if (any(grepl("Document limit reached", crawl$fetch_log$error, fixed = TRUE))) completeness <- "document limit reached"
      if (status == "ok" && nrow(crawl$additional_relationships)) {
        warning <- if (warning == "no warnings") "additional relationships not traversed" else
          paste(warning, "additional relationships not traversed", sep = "; ")
      }
      if (status == "ok" && !is.null(queue) && any(queue$traversal_status == "unsupported_type")) {
        warning <- if (warning == "no warnings") "resource types not traversed" else
          paste(warning, "resource types not traversed", sep = "; ")
      }
      if (any(grepl("Document limit reached", root$error, fixed = TRUE))) status <- "not requested: document limit"
    } else if (any(membership$selected)) status <- warning <- "not assessed"
    result <- data.frame(FDP = if (is.na(url)) "" else url,
      `Index state` = paste(unique(membership$State), collapse = "; "),
      `Crawl status` = status, `Warning type` = warning,
      `Number of FDPCrawleR warnings` = count, `Traversal completeness` = completeness,
      check.names = FALSE)
    if (length(pack$config$assessments)) {
      assessment <- if (!is.na(url) && nzchar(url)) pack$assessments[[url]] else NULL
      labels <- shared_test_labels(pack)
      for (level in names(labels)) {
        result[[paste(labels[[level]], "outcome")]] <- if (!is.null(assessment[[level]]) && nrow(assessment[[level]]))
          assessment[[level]]$outcome[[1]] else "not assessed"
      }
    }
    result
  })
  dplyr::bind_rows(rows)
}
shared_table5 <- function(pack, state = "ALL") {
  if (!length(pack$config$assessments)) return(NULL)
  result <- shared_table4(pack, state)["FDP"]
  labels <- shared_test_labels(pack)
  for (level in names(labels)) result[[paste(labels[[level]], "failure reasons")]] <- vapply(result$FDP, function(url) {
    row <- if (nzchar(url)) pack$assessments[[url]][[level]] else NULL
    if (is.null(row) || !nrow(row)) "Not assessed / no result recorded" else report_test_reason(row[1, ])
  }, character(1))
  result
}
shared_summary <- function(pack, number, state = "ALL") {
  if (number == 4L) return(dashboard_display_table4(pack, state))
  if (number == 5L) return(shared_table5(pack, state))
  dashboard_display_summary(pack, number, state)
}
shared_evidence <- function(pack, state = "ALL") {
  tables <- list(Index = shared_index(pack, state))
  selected <- intersect(shared_urls(pack, state), names(pack$crawls))
  keys <- unique(unlist(lapply(pack$crawls, names)))
  for (key in keys) {
    rows <- lapply(selected, function(url) {
      frame <- pack$crawls[[url]][[key]]
      if (!is.data.frame(frame)) return(NULL)
      frame <- report_frame(frame)
      frame$Owner_FDP <- rep(url, nrow(frame))
      frame
    })
    if (any(vapply(rows, is.data.frame, logical(1)))) tables[[key]] <- dplyr::bind_rows(rows) else {
      prototypes <- Filter(is.data.frame, lapply(pack$crawls, `[[`, key))
      if (length(prototypes)) {
        tables[[key]] <- report_frame(prototypes[[1]])[0, , drop = FALSE]
        tables[[key]]$Owner_FDP <- character()
      }
    }
  }
  if (length(pack$config$assessments)) {
    key <- if (identical(pack$config$profile, "erdera")) "ERDERA_results" else "Assessment_results"
    tables[[key]] <- shared_assessments(pack, state)
  }
  tables
}
shared_workbook <- function(pack, state = "ALL") {
  sheets <- setNames(lapply(1:4, function(i) shared_summary(pack, i, state)), paste0("Table", 1:4))
  sheets$Index <- shared_index(pack, state)
  if (length(pack$config$assessments)) sheets$Table5 <- shared_table5(pack, state)
  sheets$Notes <- data.frame(Note = c(paste("Index:", pack$config$index_url), paste("Profile:", pack$config$profile),
    paste("Saved:", pack$saved_at), "Counts describe recorded evidence. Incomplete traversal may hide further warnings.",
    "Root fetch failures have no structural warning count (NA).",
    paste("Selected Index States:", paste(shared_states(pack, state), collapse = ", ")),
    "State columns may overlap. No additional Total columns are calculated.",
    "Table 2 counts complete terminal paths, not successful HTTP requests. Failed/untraversed paths remain in evidence and Sankey.",
    "Resources - Data Services: catalog-linked services; DataServices: services reached from distributions or directly from datasets.",
    "Table4 contains one row per FDP with the original warning types and occurrence total. Long Excel cells are truncated to 32767 characters."))
  lapply(sheets, function(frame) {
    for (key in names(frame)) if (is.character(frame[[key]])) frame[[key]] <- substr(frame[[key]], 1L, 32767L)
    frame
  })
}
shared_sankey <- function(pack, state = "ALL") {
  groups <- lapply(shared_states(pack, state), function(st) {
    dashboard_paths(dashboard_crawls(pack, st), pack$config)
  })
  names(groups) <- shared_states(pack, state)
  groups <- groups[vapply(groups, nrow, integer(1)) > 0L]
  plot_fdp_paths(groups = groups, dcat_only = identical(pack$config$profile, "generic"))
}
