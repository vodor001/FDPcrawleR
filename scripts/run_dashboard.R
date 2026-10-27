args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script)) setwd(dirname(dirname(normalizePath(script[[1]]))))
source("R/bootstrap.R")
run_shared_dashboard <- function(run, ...) {
  file <- if (dir.exists(run)) file.path(run, "report.rds") else run
  read_dashboard_snapshot(file) # Validate the saved format before starting Shiny.
  project_root <- getwd()
  snapshot_file <- normalizePath(file)
  rmarkdown::run("report/dashboard.Rmd", render_args = list(params = list(
    snapshot_file = snapshot_file, project_root = project_root)), ...)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--help")) cat("Usage: Rscript scripts/run_dashboard.R --run results/PROFILE/RUN_ID\n") else {
    if (length(args) != 2L || args[[1]] != "--run") stop("Use --run RUN_DIRECTORY or --help.")
    run_shared_dashboard(args[[2]])
  }
}
