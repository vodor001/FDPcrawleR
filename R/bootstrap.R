# Activate the locked project library even when a launcher was invoked outside
# the checkout or with --vanilla. Fresh checkouts should run scripts/setup.R first.
if (!"renv" %in% loadedNamespaces() || !identical(renv::project(), normalizePath("."))) {
  source("renv/activate.R")
}
source("R/structural_summary.R")
source("R/report_helpers.R")
configure_dashboard()
source("R/index.R")
source("R/crawl.R")
source("R/plots.R")
source("R/metadata_plot.R")
source("R/dashboard_tables.R")
source("R/summaries.R")
source("R/pipeline.R")
