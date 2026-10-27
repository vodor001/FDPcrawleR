normalize_fdp_url <- function(x) sub("/+$", "", trimws(as.character(x)))
valid_fdp_url <- function(x) !is.na(x) & grepl("^https?://[^/[:space:]?#]+[^[:space:]]*$", x)

validate_pipeline_config <- function(config) {
  if (!is.list(config)) stop("Configuration must be a mapping.")
  allowed <- c("profile", "index_url", "states", "assessments", "generalized", "traversal",
               "timeout_seconds", "max_documents_per_fdp", "output_dir")
  if (any(!names(config) %in% allowed)) stop("Unknown configuration option: ", paste(setdiff(names(config), allowed), collapse = ", "))
  if (!is.character(config$profile) || length(config$profile) != 1L || is.na(config$profile) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", config$profile)) stop("Invalid profile name.")
  config$index_url <- normalize_fdp_url(config$index_url)
  if (length(config$index_url) != 1L || !valid_fdp_url(config$index_url)) stop("Invalid index URL.")
  if (!length(config$states) || !is.character(unlist(config$states)) || anyNA(unlist(config$states))) stop("Invalid states.")
  if (!is.character(config$output_dir) || length(config$output_dir) != 1L || is.na(config$output_dir) || !nzchar(config$output_dir)) stop("Invalid output_dir.")
  if (!is.null(config$generalized) && (!is.logical(config$generalized) || length(config$generalized) != 1L || is.na(config$generalized))) stop("Invalid generalized flag.")
  for (key in c("timeout_seconds", "max_documents_per_fdp")) {
    value <- config[[key]]
    if (key == "max_documents_per_fdp" && is.null(value)) next
    if (length(value) != 1L || !is.numeric(value) || is.na(value) || !is.finite(value) || value <= 0 ||
        (key == "max_documents_per_fdp" && value != floor(value))) stop("Invalid ", key)
  }
  compile_traversal(config$traversal)
  tests <- config$assessments
  if (length(tests)) {
    if (!is.list(tests) || is.null(names(tests)) || anyDuplicated(names(tests)) ||
        any(!grepl("^[a-z][a-z0-9_]*$", names(tests)))) stop("Assessments need unique test identifiers.")
    for (id in names(tests)) {
      test <- tests[[id]]
      if (!is.list(test) || any(!names(test) %in% c("label", "endpoint", "timeout_seconds"))) stop("Invalid assessment: ", id)
      if (!is.character(test$label) || length(test$label) != 1L || is.na(test$label) || !nzchar(test$label)) stop("Assessment needs a label: ", id)
      if (!is.character(test$endpoint) || length(test$endpoint) != 1L || !valid_fdp_url(test$endpoint)) stop("Invalid assessment endpoint: ", id)
      if (is.null(test$timeout_seconds)) tests[[id]]$timeout_seconds <- 120
      timeout <- tests[[id]]$timeout_seconds
      if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || !is.finite(timeout) || timeout <= 0) stop("Invalid assessment timeout: ", id)
    }
    if (anyDuplicated(vapply(tests, `[[`, character(1), "label"))) stop("Assessment labels must be unique.")
  }
  config$assessments <- tests %||% list()
  config
}

load_pipeline_config <- function(profile = "generic", index_url = NULL, config_file = NULL) {
  if (is.null(config_file)) {
    if (!profile %in% c("generic", "erdera")) stop("Use --config for a custom profile.")
    config_file <- file.path("config", paste0(profile, ".yml"))
  }
  config <- yaml::read_yaml(config_file)
  if (!is.null(index_url)) config$index_url <- index_url
  validate_pipeline_config(config)
}

fetch_index <- function(base_url) {
  response <- httr::GET(paste0(normalize_fdp_url(base_url), "/index/entries/all"),
                        httr::accept_json(), httr::timeout(30))
  httr::stop_for_status(response)
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

prepare_index <- function(entries, config) {
  if (!is.data.frame(entries) || !all(c("clientUrl", "state") %in% names(entries))) {
    stop("Index response must be a table with clientUrl and state columns.")
  }
  if (!"uuid" %in% names(entries)) entries$uuid <- NA_character_
  index <- data.frame(entry_id = seq_len(nrow(entries)), uuid = as.character(entries$uuid),
    FDP = normalize_fdp_url(entries$clientUrl), State = as.character(entries$state),
    stringsAsFactors = FALSE)
  index$State[is.na(index$State) | !nzchar(index$State)] <- "UNKNOWN"
  index$valid_url <- valid_fdp_url(index$FDP)
  index$selected <- index$State %in% unlist(config$states) | "ALL" %in% unlist(config$states)
  index$disposition <- ifelse(!index$valid_url, "invalid/missing URL",
                             ifelse(index$selected, "selected", "not selected"))
  index
}
