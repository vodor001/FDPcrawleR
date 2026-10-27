# Shared dashboard formatting, saved-snapshot loading and assessment explanations.
configure_dashboard <- function() {
  options(shiny.maxRequestSize = getOption("shiny.maxRequestSize", 512 * 1024^2))
  # Dependencies now come from the renv project library. A legacy local library
  # must not shadow the versions restored from renv.lock.
  # RStudio supplies Pandoc even when Rscript is run outside RStudio on macOS.
  if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && !nzchar(Sys.which("pandoc"))) {
    arch <- if (grepl("aarch64|arm64", R.version$arch)) "aarch64" else "x86_64"
    pandoc <- file.path("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools", arch)
    if (file.exists(file.path(pandoc, "pandoc"))) Sys.setenv(RSTUDIO_PANDOC = pandoc)
  }
  invisible(NULL)
}

report_frame <- function(x, keep_rownames = FALSE) {
  x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  if (keep_rownames) x <- data.frame(FDP = rownames(x), x, row.names = NULL, check.names = FALSE)
  for (column in names(x)) {
    if (is.list(x[[column]])) {
      x[[column]] <- vapply(x[[column]], function(value) {
        paste(as.character(unlist(value, use.names = FALSE)), collapse = "; ")
      }, character(1))
    }
  }
  x
}

report_test_reason <- function(row) {
  field <- function(name) {
    value <- row[[name]]
    if (is.null(value) || !length(value) || is.na(value[[1]])) "" else trimws(as.character(value[[1]]))
  }
  outcome <- tolower(field("outcome"))
  error <- field("error")
  status <- suppressWarnings(as.integer(field("http_status")))
  if (nzchar(error) || outcome == "error" || (!is.na(status) && (status < 200L || status >= 300L))) {
    if (!nzchar(error)) error <- if (!is.na(status) && (status < 200L || status >= 300L)) {
      paste("HTTP", status)
    } else "No error details recorded"
    return(paste0("Request error: ", sub("^Request error:\\s*", "", error, ignore.case = TRUE)))
  }
  if (outcome == "pass") return("pass")
  lines <- trimws(unlist(strsplit(field("log"), "\\r?\\n", perl = TRUE)))
  failures <- unique(lines[grepl("^(FAILURE|FAIL|ERROR):", lines, ignore.case = TRUE)])
  warnings <- unique(lines[grepl("^WARN(ING)?:", lines, ignore.case = TRUE)])
  missing_required <- warnings[grepl("(mandatory|required) metadata element", warnings, ignore.case = TRUE) &
    grepl("could not be found|missing|not found|absent", warnings, ignore.case = TRUE)]
  # Optional-field and harvesting warnings do not explain a required-field failure.
  missing_failure <- any(grepl("required metadata element.*missing|mandatory metadata element.*missing",
    failures, ignore.case = TRUE))
  details <- unique(c(failures, missing_required))
  if (!length(failures)) {
    summary <- field("summary")
    fallback <- if (nzchar(summary)) summary else "No failure details recorded"
    label <- if (outcome == "fail") "Failure" else if (nzchar(outcome)) outcome else "Outcome unavailable"
    details <- c(paste0(label, ": ", fallback), missing_required)
  }
  context <- setdiff(warnings, missing_required)
  if (!missing_failure && length(context)) {
    details <- c(details, paste0("Log warnings (context): ", paste(context, collapse = "\n")))
  }
  paste(unique(details), collapse = "\n")
}

# Dashboard snapshots contain saved crawl evidence; loading never fetches URLs.
read_dashboard_snapshot <- function(file) {
  pack <- readRDS(file)
  if (!is.list(pack) || !identical(pack$version, 2L)) {
    stop("The dashboard requires a version-2 report.rds produced by scripts/run_pipeline.R.")
  }
  if (!is.list(pack$config) || !is.character(pack$config$profile) ||
      length(pack$config$profile) != 1L || is.na(pack$config$profile) ||
      !is.character(pack$config$index_url) || length(pack$config$index_url) != 1L ||
      !is.data.frame(pack$index) ||
      !all(c("FDP", "State", "valid_url", "selected") %in% names(pack$index)) ||
      !is.list(pack$crawls) || (length(pack$crawls) && is.null(names(pack$crawls)))) {
    stop("This file is not a complete dashboard snapshot. Select a run's report.rds.")
  }
  for (crawl in pack$crawls) {
    required <- c("fdp_summary", "path_details", "metadata_case_details", "fetch_log")
    if (!is.list(crawl) || !all(vapply(crawl[required], is.data.frame, logical(1)))) {
      stop("This snapshot is missing crawl evidence. Select a complete report.rds.")
    }
  }
  pack
}

# Offer completed reports, not individual checkpoint/configuration RDS files.
dashboard_saved_reports <- function(snapshot_file, config, project_root) {
  output <- config$output_dir
  if (is.null(output)) output <- "results"
  if (!grepl("^(/|[A-Za-z]:)", output)) output <- file.path(project_root, output)
  roots <- unique(c(file.path(project_root, "results"), output, dirname(snapshot_file)))
  files <- unique(unlist(lapply(roots[dir.exists(roots)], function(root) {
    list.files(root, pattern = "^report\\.rds$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  if (!length(files)) return(setNames(character(), character()))
  files <- unique(normalizePath(files, mustWork = TRUE))
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  labels <- paste(basename(dirname(dirname(files))), basename(dirname(files)), sep = " / ")
  duplicates <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[duplicates] <- files[duplicates]
  setNames(files, labels)
}
