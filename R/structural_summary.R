# Presentation labels leave the original Table 3 evidence case names intact.
fdp_warning_labels <- c(
  "LDP-only links" = "LDP mismatch",
  "Empty LDP containers" = "Empty LDP containers",
  "No DCAT or LDP continuation" = 'No children ("dead end")'
)
format_fdp_warnings <- function(values) {
  values <- unique(unlist(strsplit(values, "; ", fixed = TRUE)))
  order <- c(unname(fdp_warning_labels), "fetch failed", "parse failed", "not assessed")
  found <- order[order %in% values]
  if (length(found)) paste(found, collapse = "; ") else "no warnings"
}

fdp_crawler_warnings <- function(fdps, crawl) {
  norm <- function(x) sub("/+$", "", trimws(as.character(x)))
  paths <- crawl$path_details
  cases <- crawl$metadata_case_details
  required <- c("fdp", "catalog_url", "resource_url", "distribution")
  if (is.null(paths) || is.null(cases) || !all(required %in% names(paths)) ||
      !all(c("parent_url", "case") %in% names(cases))) {
    stop("Crawl results need metadata_case_details and path_details to map warnings to FDPs.")
  }
  # Discovery-path ownership includes shared documents and LDP-only links.
  affected <- lapply(names(fdp_warning_labels), function(problem) {
    parents <- norm(cases$parent_url[cases$case %in% problem])
    parents <- parents[!is.na(parents) & nzchar(parents)]
    matches <- Reduce(`|`, lapply(required, function(column) norm(paths[[column]]) %in% parents))
    queue <- crawl$traversal_documents
    queue_owners <- if (!is.null(queue)) queue$fdp[norm(queue$url) %in% parents] else character()
    unique(norm(c(paths$fdp[matches], queue_owners)))
  })
  vapply(norm(fdps), function(fdp) {
    found <- vapply(affected, function(owners) fdp %in% owners, logical(1))
    warnings <- unname(fdp_warning_labels[found])
    status <- paths$fdp_status[norm(paths$fdp) %in% fdp]
    status <- unique(status[!is.na(status) & nzchar(status)])
    if (!length(status) && !is.null(crawl$fetch_log)) {
      log <- crawl$fetch_log
      status <- log$check_status[norm(log$url) %in% fdp]
      status <- unique(status[!is.na(status) & nzchar(status)])
    }
    if ("fetch_failed" %in% status) warnings <- c(warnings, "fetch failed")
    if ("parse_failed" %in% status) warnings <- c(warnings, "parse failed")
    if (!length(status) || any(!status %in% c("ok", "fetch_failed", "parse_failed"))) {
      warnings <- c(warnings, "not assessed")
    }
    format_fdp_warnings(warnings)
  }, character(1), USE.NAMES = FALSE)
}

# Count distinct structural case records owned by each FDP. Repeated paths and
# duplicate records across index groups must not inflate the count.
fdp_crawler_warning_counts <- function(fdps, crawls, warning_types) {
  norm <- function(x) sub("/+$", "", trimws(as.character(x)))
  keys <- c("Metadata Layer", "case", "parent_url", "child_url", "container_url")
  vapply(seq_along(fdps), function(i) {
    types <- strsplit(warning_types[[i]], "; ", fixed = TRUE)[[1]]
    if (any(types %in% c("fetch failed", "parse failed", "not assessed"))) return(NA_integer_)
    records <- lapply(crawls, function(crawl) {
      paths <- crawl$path_details
      paths <- paths[norm(paths$fdp) %in% norm(fdps[[i]]), , drop = FALSE]
      parents <- norm(unlist(paths[c("fdp", "catalog_url", "resource_url", "distribution")], use.names = FALSE))
      queue <- crawl$traversal_documents
      if (!is.null(queue)) parents <- c(parents, norm(queue$url[norm(queue$fdp) %in% norm(fdps[[i]])]))
      parents <- parents[!is.na(parents) & nzchar(parents)]
      cases <- crawl$metadata_case_details
      cases <- cases[cases$case %in% names(fdp_warning_labels) & norm(cases$parent_url) %in% parents, , drop = FALSE]
      for (key in setdiff(keys, names(cases))) cases[[key]] <- rep(NA_character_, nrow(cases))
      cases <- cases[keys]
      for (key in c("parent_url", "child_url", "container_url")) cases[[key]] <- norm(cases[[key]])
      cases
    })
    as.integer(nrow(unique(do.call(rbind, records))))
  }, integer(1))
}

