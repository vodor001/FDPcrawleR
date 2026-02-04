# fdp_layers_table.R
# Builds a per-FDP "metadata layers coverage" table from crawl_fdp() connections.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# --- small helper: normalize URLs consistently (safe even if already normalized) ---
normalize_url_safe <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("/+$", "", x)               # remove trailing slashes
  x <- gsub("^http://", "https://", x)  # optional: align schemes if you do this elsewhere
  x
}

# --- helper to summarize unique values into a list-column + count ---
summarise_list_count <- function(df, key_col, value_col, out_list_name, out_n_name) {
  df %>%
    group_by(.data[[key_col]]) %>%
    summarise(
      "{out_list_name}" := list(unique(.data[[value_col]])),
      "{out_n_name}" := n_distinct(.data[[value_col]]),
      .groups = "drop"
    )
}

# --- main: build per-FDP table for ONE run (active OR inactive) ---
build_fdp_layer_table <- function(fdp_id_url_df,
                                  connections,
                                  status_label = "ACTIVE",
                                  normalize_fn = normalize_url_safe) {
  stopifnot(all(c("id", "url") %in% names(fdp_id_url_df)))
  
  base <- tibble(
    id = fdp_id_url_df$id,
    url = normalize_fn(fdp_id_url_df$url),
    status = status_label
  )
  
  # Expect these connection tables (names match the pattern you described)
  # connections$fdp_to_catalog: fdp, catalog
  # connections$catalog_to_dataset: catalog, dataset
  # connections$dataset_to_distribution: dataset, distribution
  # connections$distribution_to_dataservice: distribution, dataservice
  
  f2c_raw <- connections$fdp_to_catalog %>%
    transmute(
      fdp = normalize_fn(.data$fdp),
      catalog = normalize_fn(.data$catalog)
    )
  
  c2d_raw <- connections$catalog_to_dataset %>%
    transmute(
      catalog = normalize_fn(.data$catalog),
      dataset = normalize_fn(.data$dataset)
    )
  
  d2dist_raw <- connections$dataset_to_distribution %>%
    transmute(
      dataset = normalize_fn(.data$dataset),
      distribution = normalize_fn(.data$distribution)
    )
  
  dist2srv_raw <- connections$distribution_to_dataservice %>%
    transmute(
      distribution = normalize_fn(.data$distribution),
      dataservice = normalize_fn(.data$dataservice)
    )
  
  # Catalog level
  f2c <- summarise_list_count(
    f2c_raw, "fdp", "catalog",
    out_list_name = "catalog_urls",
    out_n_name = "n_catalogs"
  )
  
  # Dataset level (via catalog)
  f2d_raw <- f2c_raw %>%
    inner_join(c2d_raw, by = "catalog") %>%
    select(fdp, dataset)
  
  f2d <- summarise_list_count(
    f2d_raw, "fdp", "dataset",
    out_list_name = "dataset_urls",
    out_n_name = "n_datasets"
  )
  
  # Distribution level (via dataset)
  f2dist_raw <- f2d_raw %>%
    inner_join(d2dist_raw, by = "dataset") %>%
    select(fdp, distribution)
  
  f2dist <- summarise_list_count(
    f2dist_raw, "fdp", "distribution",
    out_list_name = "distribution_urls",
    out_n_name = "n_distributions"
  )
  
  # Data service level (via distribution)
  f2srv_raw <- f2dist_raw %>%
    inner_join(dist2srv_raw, by = "distribution") %>%
    select(fdp, dataservice)
  
  f2srv <- summarise_list_count(
    f2srv_raw, "fdp", "dataservice",
    out_list_name = "dataservice_urls",
    out_n_name = "n_dataservices"
  )
  
  # Build one row per FDP
  out <- base %>%
    left_join(f2c, by = c("url" = "fdp")) %>%
    left_join(f2d, by = c("url" = "fdp")) %>%
    left_join(f2dist, by = c("url" = "fdp")) %>%
    left_join(f2srv, by = c("url" = "fdp")) %>%
    mutate(
      n_catalogs      = dplyr::coalesce(.data$n_catalogs, 0L),
      n_datasets      = dplyr::coalesce(.data$n_datasets, 0L),
      n_distributions = dplyr::coalesce(.data$n_distributions, 0L),
      n_dataservices  = dplyr::coalesce(.data$n_dataservices, 0L),
      
      has_catalog      = .data$n_catalogs > 0,
      has_dataset      = .data$n_datasets > 0,
      has_distribution = .data$n_distributions > 0,
      has_dataservice  = .data$n_dataservices > 0,
      has_all_layers   = has_catalog & has_dataset & has_distribution & has_dataservice
      )
  
  out
}

# --- merge ACTIVE + INACTIVE tables into a single per-FDP table ---
merge_layer_runs <- function(layers_active, layers_inactive, normalize_fn = normalize_url_safe) {
  dplyr::bind_rows(layers_active, layers_inactive) %>%
    mutate(url = normalize_fn(.data$url)) %>%
    group_by(.data$url) %>%
    summarise(
      id = dplyr::first(.data$id[!is.na(.data$id)]),
      status = paste(sort(unique(.data$status)), collapse = "+"),
      
      has_catalog      = any(.data$has_catalog, na.rm = TRUE),
      has_dataset      = any(.data$has_dataset, na.rm = TRUE),
      has_distribution = any(.data$has_distribution, na.rm = TRUE),
      has_dataservice  = any(.data$has_dataservice, na.rm = TRUE),
      has_all_layers   = any(.data$has_all_layers, na.rm = TRUE),
      
      catalog_urls      = list(unique(unlist(.data$catalog_urls))),
      dataset_urls      = list(unique(unlist(.data$dataset_urls))),
      distribution_urls = list(unique(unlist(.data$distribution_urls))),
      dataservice_urls  = list(unique(unlist(.data$dataservice_urls))),
      
      n_catalogs      = max(.data$n_catalogs, na.rm = TRUE),
      n_datasets      = max(.data$n_datasets, na.rm = TRUE),
      n_distributions = max(.data$n_distributions, na.rm = TRUE),
      n_dataservices  = max(.data$n_dataservices, na.rm = TRUE)) %>%
    mutate(
      has_all_layers = has_catalog & has_dataset & has_distribution & has_dataservice
    )
}
