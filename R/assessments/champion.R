# FAIR Champion JSON-LD TestResult adapter. Endpoints are supplied by config.
# Every configured test receives the FDP root URL and runs independently.

.champion_value <- function(x) {
  if (is.list(x)) x <- x[["@value"]]
  if (is.null(x) || length(x) != 1L) return(NA_character_)
  as.character(x)
}

.champion_parse <- function(body) {
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  graph <- payload[["@graph"]]
  results <- Filter(function(node) {
    any(unlist(node[["@type"]]) %in%
          c("ftr:TestResult", "https://w3id.org/ftr#TestResult"))
  }, graph)
  if (length(results) != 1L) {
    stop("Expected one TestResult in the service's JSON-LD response.")
  }
  node <- results[[1L]]
  field <- function(compact, expanded) {
    value <- node[[compact]]
    if (is.null(value)) value <- node[[expanded]]
    .champion_value(value)
  }
  outcome <- tolower(field("prov:value", "http://www.w3.org/ns/prov#value"))
  if (is.na(outcome) || !outcome %in% c("pass", "fail", "indeterminate")) {
    stop("The service did not return pass, fail, or indeterminate.")
  }
  list(
    outcome = outcome,
    summary = field("ftr:summary", "https://w3id.org/ftr#summary"),
    log = field("ftr:log", "https://w3id.org/ftr#log")
  )
}

.champion_post <- function(endpoint, resource_identifier, timeout_seconds) {
  response <- httr::POST(
    endpoint,
    httr::accept_json(),
    httr::timeout(timeout_seconds),
    body = list(resource_identifier = resource_identifier),
    encode = "json"
  )
  list(
    status = httr::status_code(response),
    body = httr::content(response, as = "text", encoding = "UTF-8")
  )
}

champion_tests <- function(fdp_list, tests, state = "ALL", verbose = TRUE,
                           post_fun = .champion_post) {
  for (package in c("httr", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Install the required package: ", package)
    }
  }
  urls <- trimws(as.character(fdp_list))
  urls <- unique(urls[!is.na(urls) & nzchar(urls)])
  if (any(!grepl("^https?://[^/[:space:]]+[^[:space:]]*$", urls))) {
    stop("fdp_list must contain HTTP or HTTPS URLs.")
  }

  endpoints <- vapply(tests, `[[`, character(1), "endpoint")
  empty <- data.frame(
    fdp = character(), state = character(), level = character(),
    endpoint = character(), outcome = character(), http_status = integer(),
    summary = character(), log = character(), error = character(),
    tested_at = character(), response_body = character(),
    stringsAsFactors = FALSE
  )

  run_level <- function(level) {
    if (!length(urls)) return(empty)
    rows <- lapply(seq_along(urls), function(i) {
      if (verbose) message(state, " ", toupper(level), " [", i, "/",
                           length(urls), "]: ", urls[[i]])
      row <- data.frame(
        fdp = urls[[i]], state = state, level = level,
        endpoint = unname(endpoints[[level]]), outcome = "error",
        http_status = NA_integer_, summary = NA_character_, log = NA_character_,
        error = NA_character_,
        tested_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        response_body = NA_character_, stringsAsFactors = FALSE
      )
      # Request/HTTP/parse errors are distinct from a compliance failure.
      # Preserve the raw response for inspection even if parsing fails.
      tryCatch({
        response <- post_fun(endpoints[[level]], urls[[i]], tests[[level]]$timeout_seconds)
        row$http_status <- response$status
        row$response_body <- as.character(response$body)
        if (response$status < 200L || response$status >= 300L) {
          stop("HTTP ", response$status)
        }
        parsed <- .champion_parse(response$body)
        row$outcome <- parsed$outcome
        row$summary <- parsed$summary
        row$log <- parsed$log
        row
      }, error = function(e) {
        row$error <- conditionMessage(e)
        row
      })
    })
    do.call(rbind, rows)
  }

  setNames(lapply(names(tests), run_level), names(tests))
}
