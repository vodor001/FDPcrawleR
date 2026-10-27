# Restore the exact project dependency versions. System libraries and Pandoc
# must be available first; see README.md. Global R libraries are not modified.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script)) setwd(dirname(dirname(normalizePath(script[[1]]))))
if (!"renv" %in% loadedNamespaces() || !identical(renv::project(), normalizePath("."))) {
  source("renv/activate.R")
}
renv::restore(prompt = FALSE)
status <- renv::status()
if (!isTRUE(status$synchronized)) stop("Project library does not match renv.lock.")
cat("Project dependencies restored from renv.lock. See README.md for launch commands.\n")
