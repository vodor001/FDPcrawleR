invocation_dir <- getwd()
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script)) setwd(dirname(dirname(normalizePath(script[[1]]))))
source("R/bootstrap.R")
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args) {
  cat("Usage: Rscript scripts/run_pipeline.R [--profile generic|erdera | --config FILE] [--index-url URL] [--output-dir DIR] [--resume RUN_DIR]\n")
} else {
  if (length(args) %% 2L) stop("Arguments must be --option value pairs. Use --help.")
  options <- if (length(args)) setNames(as.list(args[seq(2L, length(args), 2L)]), args[seq(1L, length(args), 2L)]) else list()
  if (any(!names(options) %in% c("--profile", "--config", "--index-url", "--output-dir", "--resume")) || anyDuplicated(names(options))) stop("Unknown or repeated option. Use --help.")
  if (all(c("--profile", "--config") %in% names(options))) stop("Use --profile or --config, not both.")
  config_file <- options[["--config"]]
  if (!is.null(config_file) && !grepl("^(/|[A-Za-z]:)", config_file)) config_file <- file.path(invocation_dir, config_file)
  profile <- if (is.null(options[["--profile"]])) "generic" else options[["--profile"]]
  run_pipeline(profile = profile, index_url = options[["--index-url"]],
    output_dir = options[["--output-dir"]], resume = options[["--resume"]], config_file = config_file)
}
