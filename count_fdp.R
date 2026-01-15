count_fdp <- function(fdp_active_list, 
                      fdp_to_catalog, 
                      catalog_to_dataset, 
                      dataset_to_distribution, 
                      distribution_to_dataservice) {

all_fdp <- unique(normalize_url(fdp_active_list))
n_all  <- length(all_fdp)

# Helper: keep only FDPs that are in the index list (prevents overcounts)
as_index_fdp <- function(x) intersect(unique(normalize_url(x)), all_fdp)

# FDP with at least 1 Catalog
fdp_with_catalog <- fdp_to_catalog %>%
  distinct(fdp) %>%
  pull(fdp) %>%
  as_index_fdp()

# FDP without Catalog
fdp_without_catalog <- setdiff(all_fdp, fdp_with_catalog)

# FDP with at least one Dataset (via Catalog)
fdp_with_dataset <- fdp_to_catalog %>%
  inner_join(catalog_to_dataset, by = "catalog") %>%
  distinct(fdp) %>%
  pull(fdp) %>%
  as_index_fdp()

# FDP with Catalog, but without Dataset
fdp_with_catalog_no_dataset <- setdiff(fdp_with_catalog, fdp_with_dataset)

# FDP with at least one Distribution (via Dataset)
datasets_with_distribution <- dataset_to_distribution %>%
  distinct(dataset) %>%
  pull(dataset) %>%
  unique() %>%
  normalize_url()

fdp_with_distribution <- fdp_to_catalog %>%
  inner_join(catalog_to_dataset,  by = "catalog") %>%
  filter(normalize_url(dataset) %in% datasets_with_distribution) %>%
  distinct(fdp) %>%
  pull(fdp) %>%
  as_index_fdp()

# FDP with Dataset, but without Distribution
fdp_with_dataset_no_distribution <- setdiff(fdp_with_dataset
                                            , fdp_with_distribution)

# Distributions that have at least one DataService
distributions_with_dataservice <- distribution_to_dataservice %>%
  distinct(distribution) %>%
  pull(distribution) %>%
  unique() %>%
  normalize_url()

# FDP with at least one DataService (via Dataset -> Distribution -> DataService)
fdp_with_dataservice <- fdp_to_catalog %>%
  inner_join(catalog_to_dataset,      by = "catalog") %>%
  inner_join(dataset_to_distribution, by = "dataset") %>%
  filter(normalize_url(distribution) %in% distributions_with_dataservice) %>%
  distinct(fdp) %>%
  pull(fdp) %>%
  as_index_fdp()

# FDP with Distribution but without DataService
fdp_with_distribution_no_dataservice <- setdiff(fdp_with_distribution
                                                , fdp_with_dataservice)


#============================

counts <- list(
  
  n_all        = length(fdp_active_list),
  n_cat_yes    = length(fdp_with_catalog),
  n_cat_no     = length(fdp_without_catalog),
  
  n_ds_yes     = length(fdp_with_dataset),
  n_ds_no      = length(fdp_with_catalog_no_dataset),
  
  n_dist_yes   = length(fdp_with_distribution),
  n_dist_no    = length(fdp_with_dataset_no_distribution),
  
  n_dsrv_yes = length(fdp_with_dataservice),
  n_dsrv_no  = length(fdp_with_distribution_no_dataservice)
)


return(
  counts = counts)
}

