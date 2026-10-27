# Each root is a checkpoint: duplicated index membership does not refetch it.
# Rerun the exact command with --resume <run-directory> after an interruption.
run_pipeline <- function(profile = "generic", index_url = NULL, output_dir = NULL,
                         resume = NULL, index_fetch = fetch_index,
                         metadata_fetch = NULL, endpoint_fetch = NULL,
                         assessment_runner = NULL, config = NULL, config_file = NULL) {
  if (!is.null(config) && !is.null(config_file)) stop("Pass config or config_file, not both.")
  if (is.null(config)) config <- load_pipeline_config(profile, index_url, config_file)
  config <- validate_pipeline_config(config)
  if (is.null(output_dir)) output_dir <- config$output_dir
  source_files <- c(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
                    list.files("config", full.names = TRUE), "report/dashboard.Rmd",
                    "DESCRIPTION", "renv.lock",
                    ".Rprofile", "renv/settings.json", "renv/activate.R")
  fingerprint <- tools::md5sum(source_files)
  if (is.null(resume)) {
    run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "-", Sys.getpid())
    run_dir <- file.path(output_dir, config$profile, run_id)
    if (dir.exists(run_dir)) stop("Run directory already exists: ", run_dir)
    dir.create(run_dir, recursive = TRUE)
    raw_index <- index_fetch(config$index_url)
    index <- prepare_index(raw_index, config)
    saveRDS(fingerprint, file.path(run_dir, "source_versions.rds"))
    saveRDS(config, file.path(run_dir, "config.rds"))
    saveRDS(raw_index, file.path(run_dir, "index_raw.rds"))
    saveRDS(index, file.path(run_dir, "index.rds"))
  } else {
    run_dir <- resume
    saved <- readRDS(file.path(run_dir, "config.rds"))
    if (!identical(readRDS(file.path(run_dir, "source_versions.rds")), fingerprint)) {
      stop("Source files changed since this run; start a new run to avoid mixing implementations.")
    }
    if (!identical(saved, config)) stop("Resume requires the original configuration.")
    index <- readRDS(file.path(run_dir, "index.rds"))
  }
  dir.create(file.path(run_dir, "checkpoints"), showWarnings = FALSE)
  atomic_save <- function(value, path) {
    temporary <- paste0(path, ".tmp")
    saveRDS(value, temporary)
    if (!file.rename(temporary, path)) stop("Could not save checkpoint: ", path)
  }
  fdps <- unique(index$FDP[index$disposition == "selected"])
  crawls <- assessments <- setNames(vector("list", length(fdps)), fdps)
  if (length(config$assessments) && is.null(assessment_runner)) {
    source("R/assessments/champion.R", local = TRUE)
    assessment_runner <- function(url) champion_tests(url, config$assessments)
  }
  for (i in seq_along(fdps)) {
    url <- fdps[[i]]
    message("FDP ", i, "/", length(fdps), ": ", url)
    checkpoint <- file.path(run_dir, "checkpoints", sprintf("%05d.rds", i))
    if (file.exists(checkpoint)) {
      record <- readRDS(checkpoint)
      if (!identical(record$url, url)) stop("Checkpoint URL does not match saved index.")
    } else {
      attempts <- 0L
      bounded_fetch <- function(target) {
        attempts <<- attempts + 1L
        if (!is.null(config$max_documents_per_fdp) && attempts > config$max_documents_per_fdp) stop("Document limit reached; not requested")
        if (!is.null(metadata_fetch)) return(metadata_fetch(target))
        response <- httr::GET(target, httr::timeout(config$timeout_seconds),
                               httr::add_headers(Accept = "text/turtle"))
        list(status_code = httr::status_code(response), url = response$url,
             body = httr::content(response, as = "text", encoding = "UTF-8"))
      }
      endpoint_fun <- if (!is.null(endpoint_fetch)) endpoint_fetch else function(target) {
        response <- httr::GET(target, httr::timeout(config$timeout_seconds), httr::add_headers(Accept = "*/*"))
        list(status_code = httr::status_code(response), url = response$url)
      }
      result <- crawl_fdp_clean(url, timeout_seconds = config$timeout_seconds,
        fetch_fun = bounded_fetch, endpoint_fetch_fun = endpoint_fun,
        generalized = isTRUE(config$generalized), traversal = config$traversal %||% list())
      if (!url %in% result$fdp_summary$fdp || !url %in% result$fetch_log$url) {
        stop("Crawler omitted a requested root: ", url)
      }
      result$graph <- NULL # External RDF pointers cannot be restored reliably.
      record <- list(url = url, crawl = result, assessed = FALSE,
        crawl_completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
      atomic_save(record, checkpoint)
    }
    if (length(config$assessments) && !isTRUE(record$assessed)) {
      record$assessment <- assessment_runner(url)
      # Index state is stored separately; this result is shared across memberships.
      for (level in names(record$assessment)) record$assessment[[level]]$state <- "ALL"
      record$assessed <- TRUE
      atomic_save(record, checkpoint)
    }
    crawls[[url]] <- record$crawl
    if (length(config$assessments)) assessments[[url]] <- record$assessment
  }
  if (!setequal(names(crawls), fdps)) stop("Run does not cover every selected FDP.")
  package_names <- c("R", "dplyr", "rdflib", "redland", "httr", "jsonlite", "shiny", "rmarkdown", "DT", "plotly")
  versions <- vapply(package_names, function(x) if (x == "R") as.character(getRversion()) else
    if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else NA_character_, character(1))
  pack <- list(version = 2L, config = config, index = index, crawls = crawls,
    assessments = if (length(config$assessments)) assessments else NULL,
    saved_at = format(Sys.time(), tz = "UTC", usetz = TRUE), package_versions = versions,
    source_md5 = fingerprint, run_dir = normalizePath(run_dir))
  atomic_save(pack, file.path(run_dir, "report.rds"))
  message("Saved run: ", normalizePath(run_dir))
  invisible(pack)
}
