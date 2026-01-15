library(httr)
library(rdflib)
library(dplyr)
library(plotly)

res <- GET(
  "https://index.vp.ejprarediseases.org/index/entries/all",
  add_headers(Accept = "*/*")
)

txt <- content(res, as = "text", encoding = "UTF-8")
fdp_df <- jsonlite::fromJSON(txt, flatten = TRUE)

fdp_active <- fdp_df %>%
  filter(state == "ACTIVE") 

fdp_inactive <- fdp_df %>%
  filter(state == "INACTIVE")

fdp_active_list_raw <- fdp_active$clientUrl  # list of FDP-URL

fdp_inactive_list_raw <- fdp_inactive$clientUrl  # list of FDP-URL

#normalize all URLs - control "/" at the end - delete them

normalize_url <- function(x) {
  x <- sub("/+$", "", x)  
  x
}

fdp_active_list <- normalize_url(fdp_active_list_raw)
fdp_inactive_list <- normalize_url(fdp_inactive_list_raw)

#2==================

g_all <- rdf()
index_uri   <- "https://index.vp.ejprarediseases.org/index/"
pred_hasFdp   <- "http://example.org/hasFdp"
pred_fdpState <- "http://example.org/fdpState"

for (fdp in fdp_active_list) {
  rdf_add(g_all, index_uri, pred_hasFdp, fdp)
  rdf_add(g_all, fdp, pred_fdpState, "ACTIVE")
}

for (fdp in fdp_inactive_list) {
  rdf_add(g_all, index_uri, pred_hasFdp, fdp)
  rdf_add(g_all, fdp, pred_fdpState, "INACTIVE")
}

#check - make a table of active and inactive FDPs

fdp_state_table <- rdf_query(
  g_all,
  '
  SELECT ?fdp ?state
  WHERE {
    ?fdp <http://example.org/fdpState> ?state .
  }
  '
)



#3A=======================================
source("crawl_fdp.R")

result_active <- crawl_fdp(fdp_list = fdp_active_list)
result_inactive <- crawl_fdp(fdp_list = fdp_inactive_list)

#4A=============================================

source("count_fdp.R")

counts <- count_fdp(fdp_active_list, 
                    result_active$connections$fdp_to_catalog,
                    result_active$connections$catalog_to_dataset,
                    result_active$connections$dataset_to_distribution,
                    result_active$connections$distribution_to_dataservice)

counts_i <- count_fdp(fdp_inactive_list, 
                      result_inactive$connections$fdp_to_catalog,
                      result_inactive$connections$catalog_to_dataset,
                      result_inactive$connections$dataset_to_distribution,
                      result_inactive$connections$distribution_to_dataservice)

#5=========================================

#plotting

#unpack active

n_all      <- counts$n_all
n_cat_yes  <- counts$n_cat_yes
n_cat_no   <- counts$n_cat_no

n_ds_yes   <- counts$n_ds_yes
n_ds_no    <- counts$n_ds_no

n_dist_yes <- counts$n_dist_yes
n_dist_no  <- counts$n_dist_no

n_dsrv_yes <- counts$n_dsrv_yes
n_dsrv_no  <- counts$n_dsrv_no

#unpack inactive

n_all_i      <- counts_i$n_all

n_cat_yes_i  <- counts_i$n_cat_yes
n_cat_no_i   <- counts_i$n_cat_no

n_ds_yes_i   <- counts_i$n_ds_yes
n_ds_no_i    <- counts_i$n_ds_no

n_dist_yes_i <- counts_i$n_dist_yes
n_dist_no_i  <- counts_i$n_dist_no

n_dsrv_yes_i <- counts_i$n_dsrv_yes
n_dsrv_no_i  <- counts_i$n_dsrv_no


# --- Combined ACTIVE + INACTIVE Sankey ---

n_all_total <- n_all + n_all_i

nodes <- data.frame(
  name = c(
    paste0("All FDP\n(n = ", n_all_total, ")"),                    # 0
    paste0("ACTIVE FDP\n(n = ", n_all, ")"),                       # 1
    paste0("INACTIVE FDP\n(n = ", n_all_i, ")"),                   # 2

    paste0("ACTIVE: with Catalog\n(n = ", n_cat_yes, ")"),         # 3
    paste0("ACTIVE: without Catalog\n(n = ", n_cat_no, ")"),       # 4
    paste0("ACTIVE: with Dataset\n(n = ", n_ds_yes, ")"),          # 5
    paste0("ACTIVE: without Dataset\n(n = ", n_ds_no, ")"),        # 6
    paste0("ACTIVE: with Distribution\n(n = ", n_dist_yes, ")"),   # 7
    paste0("ACTIVE: without Distribution\n(n = ", n_dist_no, ")"), # 8
    paste0("ACTIVE: with Data Service\n(n = ", n_dsrv_yes, ")"),   # 9
    paste0("ACTIVE: without Data Service\n(n = ", n_dsrv_no, ")"), # 10

    paste0("INACTIVE: with Catalog\n(n = ", n_cat_yes_i, ")"),         # 11
    paste0("INACTIVE: without Catalog\n(n = ", n_cat_no_i, ")"),       # 12
    paste0("INACTIVE: with Dataset\n(n = ", n_ds_yes_i, ")"),          # 13
    paste0("INACTIVE: without Dataset\n(n = ", n_ds_no_i, ")"),        # 14
    paste0("INACTIVE: with Distribution\n(n = ", n_dist_yes_i, ")"),   # 15
    paste0("INACTIVE: without Distribution\n(n = ", n_dist_no_i, ")"), # 16
    paste0("INACTIVE: with Data Service\n(n = ", n_dsrv_yes_i, ")"),   # 17
    paste0("INACTIVE: without Data Service\n(n = ", n_dsrv_no_i, ")")  # 18
  )
)

# Colors: keep your active palette; make inactive lane grey; Data Service = orange
node_colors <- c(
  "#10264a",  # 0 All FDP

  "#6A9DE8",  # 1 ACTIVE
  "#99abc7",  # 2 INACTIVE

  "#083680",  # 3 ACTIVE with Catalog
  "#767c85",  # 4 ACTIVE without Catalog
  "#a1c2f0",  # 5 ACTIVE with Dataset
  "#767c85",  # 6 ACTIVE without Dataset
  "#ad7b23",  # 7 ACTIVE with Distribution
  "#767c85",  # 8 ACTIVE without Distribution
  "#E67E22",  # 9 ACTIVE with Data Service (orange)
  "#767c85",  # 10 ACTIVE without Data Service

  "#4c6185",  # 11 INACTIVE with Catalog
  "#a2a6ab",  # 12 INACTIVE without Catalog
  "#c3d3eb",  # 13 INACTIVE with Dataset
  "#a2a6ab",  # 14 INACTIVE without Dataset
  "#ab9878",  # 15 INACTIVE with Distribution
  "#a2a6ab",  # 16 INACTIVE without Distribution
  "#e3b388",  # 17 INACTIVE with Data Service (orange)
  "#D0D3D6"   # 18 INACTIVE without Data Service
)

# # X positions (columns)
# node_x <- c(
#   0.00,  # 0 All
#   0.15,  # 1 ACTIVE
#   0.15,  # 2 INACTIVE
#   0.35,  # 3 ACTIVE with Catalog
#   0.35,  # 4 ACTIVE without Catalog
#   0.55,  # 5 ACTIVE with Dataset
#   0.55,  # 6 ACTIVE without Dataset
#   0.75,  # 7 ACTIVE with Distribution
#   0.75,  # 8 ACTIVE without Distribution
#   0.95,  # 9 ACTIVE with Data Service 
#   0.95,  # 10 ACTIVE without Data Service
#   0.35,  # 11 INACTIVE with Catalog
#   0.35,  # 12 12 INACTIVE without Catalog
#   0.55,  # 13 INACTIVE with Dataset
#   0.55,  # 14 INACTIVE without Dataset
#   0.75,  # 15 INACTIVE with Distribution
#   0.75,  # 16 INACTIVE without Distribution
#   0.95,  # 17 INACTIVE with Data Service
#   0.95   # 18 INACTIVE without Data Service
# )

# Y positions (two lanes: ACTIVE bottom, INACTIVE top)
# node_y <- c(
#   0.50,  # 0 All
# 
#   0.75,  # 1 ACTIVE
#   0.25,  # 2 INACTIVE
# 
#   0.85,  # 3 ACTIVE with Catalog
#   0.65,  # 4 ACTIVE without Catalog
#   0.90,  # 5 ACTIVE with Dataset
#   0.75,  # 6 ACTIVE without Dataset
#   0.92,  # 7 ACTIVE with Distribution
#   0.80,  # 8 ACTIVE without Distribution
#   0.95,  # 9 ACTIVE with Data Service
#   0.85,  # 10 ACTIVE without Data Service
# 
#   0.35,  # 11 INACTIVE with Catalog
#   0.15,  # 12 INACTIVE without Catalog
#   0.40,  # 13 INACTIVE with Dataset
#   0.25,  # 14 INACTIVE without Dataset
#   0.42,  # 15 INACTIVE with Distribution
#   0.30,  # 16 INACTIVE without Distribution
#   0.45,  # 17 INACTIVE with Data Service
#   0.35   # 18 INACTIVE without Data Service
# )

links <- data.frame(
  source = c(
    0, 0,                 # All -> Active/Inactive

    1, 1,                 # Active -> Catalog yes/no
    3, 3,                 # Active with Catalog -> Dataset yes/no
    5, 5,                 # Active with Dataset -> Distribution yes/no
    7, 7,                 # Active with Distribution -> DataService yes/no

    2, 2,                 # Inactive -> Catalog yes/no
    11, 11,               # Inactive with Catalog -> Dataset yes/no
    13, 13,               # Inactive with Dataset -> Distribution yes/no
    15, 15                # Inactive with Distribution -> DataService yes/no
  ),
  target = c(
    1, 2,

    3, 4,
    5, 6,
    7, 8,
    9, 10,

    11, 12,
    13, 14,
    15, 16,
    17, 18
  ),
  value = c(
    n_all, n_all_i,

    n_cat_yes, n_cat_no,
    n_ds_yes, n_ds_no,
    n_dist_yes, n_dist_no,
    n_dsrv_yes, n_dsrv_no,

    n_cat_yes_i, n_cat_no_i,
    n_ds_yes_i, n_ds_no_i,
    n_dist_yes_i, n_dist_no_i,
    n_dsrv_yes_i, n_dsrv_no_i
  )
)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = nodes$name,
    pad = 15,
    thickness = 20,
    #x = node_x,
    #y = node_y,
    color = node_colors
  ),
  link = list(
    source = links$source,
    target = links$target,
    value  = links$value
  )
)

fig

