crawl_fdp <- function(fdp_list) {
  
  #helper function
  fetch_turtle <- function(url) {
    message("  Fetching: ", url)
    
    out <- tryCatch(
      {
        res <- httr::GET(
          url,
          httr::timeout(10),              # longer timeout is possible
          httr::add_headers(Accept = "text/turtle")
        )
        
        if (httr::status_code(res) >= 400) {
          message("  HTTP error ", httr::status_code(res), " for ", url)
          return(NA_character_)
        }
        
        txt <- httr::content(res, as = "text", encoding = "UTF-8")
        if (!nzchar(txt)) {
          message("  Empty body from ", url)
          return(NA_character_)
        }
        
        txt
      },
      error = function(e) {
        message("  FETCH ERROR for ", url, " : ", e$message)
        return(NA_character_)
      }
    )
    
    out
  }
  
  
  fdp_failed <- character()  # for FDPs that cannot be parsed
  
  for (fdp_url in fdp_list) {
    message("-----")
    message("Processing FDP: ", fdp_url)
    
    turtle_txt <- fetch_turtle(fdp_url)
    if (is.na(turtle_txt)) {
      fdp_failed <- c(fdp_failed, fdp_url)
      next
    }
    
    # adding new turtle to the graph
    tryCatch(
      {
        g_all <- rdf_parse(
          doc    = turtle_txt,
          format = "turtle",
          rdf    = g_all
        )
      },
      error = function(e) {
        message("  Parse error: ", e$message)
        fdp_failed <- c(fdp_failed, fdp_url)
      }
    )
  }
  
  #5=======================================
  
  df_triples <- rdf_query(
    g_all,
    'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  )
  
  #6======================================
  
  P_METADATA_CATALOG <- "https://w3id.org/fdp/fdp-o#metadataCatalog"
  
  fdp_to_catalog <- df_triples %>%
    filter(p == P_METADATA_CATALOG) %>%
    transmute(
      fdp     = s,
      catalog = o
    ) %>%
    distinct()
  
  catalog_urls_raw <- unique(fdp_to_catalog$catalog)
  catalog_urls <- normalize_url(catalog_urls_raw)
  #7==========================================
  
  catalog_failed <- character()
  
  for (cat_url in catalog_urls) {
    message("-----")
    message("Processing Catalog: ", cat_url)
    
    # Try to fetch turtle
    turtle_txt <- fetch_turtle(cat_url)
    
    # If fetch_turtle() failed - no dataset for this catalog
    if (is.na(turtle_txt)) {
      message("  Cannot fetch catalog RDF — marking as having NO datasets.")
      next
    }
    
    # Try to parse and add to RDF graph
    tryCatch(
      {
        g_all <- rdf_parse(
          doc    = turtle_txt,
          format = "turtle",
          rdf    = g_all
        )
      },
      error = function(e) {
        message("  Parse error: ", e$message)
        catalog_failed <- c(catalog_failed, cat_url)
        next
      }
    )
  }
  
  #8====================================
  df_triples <- rdf_query(
    g_all,
    'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  ) %>% 
    distinct()
  
  #9====================================
  #let's find dataset URLs
  
  P_DCAT_DATASET <- "http://www.w3.org/ns/dcat#dataset"
  
  catalog_to_dataset <- df_triples %>%
    filter(p == P_DCAT_DATASET) %>%
    transmute(
      catalog = s,
      dataset = o
    ) %>%
    distinct()
  
  dataset_urls_raw <- unique(catalog_to_dataset$dataset)
  
  dataset_urls <- normalize_url(dataset_urls_raw)
  #10=====================================
  
  dataset_failed <- character()
  for (dat_url in dataset_urls) {
    message("-----")
    message("Processing Dataset: ", dat_url)
    
    # Try to fetch turtle
    turtle_txt <- fetch_turtle(dat_url)
    
    # If fetch_turtle() failed → no dataset for this catalog
    if (is.na(turtle_txt)) {
      message("  Cannot fetch dataset RDF — marking as having NO distributions.")
      dataset_failed <- c(dataset_failed, dat_url)
      next
    }
    
    # Try to parse and add to RDF graph
    tryCatch(
      {
        g_all <- rdf_parse(
          doc    = turtle_txt,
          format = "turtle",
          rdf    = g_all
        )
      },
      error = function(e) {
        message("  Parse error: ", e$message)
        dataset_failed <- c(dataset_failed, dat_url)
        next
      }
    )
  }
  
  #11=============================
  
  df_triples <- rdf_query(
    g_all,
    'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  ) %>% 
    distinct()
  
  #12=======================================
  #let's find distribution URLs
  P_DCAT_DISTRIBUTION <- "http://www.w3.org/ns/dcat#distribution"
  
  dataset_to_distribution <- df_triples %>%
    filter(p == P_DCAT_DISTRIBUTION) %>%
    transmute(
      dataset     = s,
      distribution = o
    ) %>%
    distinct()
  
  distribution_urls_raw <- unique(dataset_to_distribution$distribution)
  distribution_urls <- normalize_url(distribution_urls_raw)
  
  #13==========================================
  
  distribution_failed <- character()
  for (distr_url in distribution_urls) {
    message("-----")
    message("Processing Distribution: ", distr_url)
    
    # Try to fetch turtle
    turtle_txt <- fetch_turtle(distr_url)
    
    # If fetch_turtle() failed → no dataset for this catalog
    if (is.na(turtle_txt)) {
      message("  Cannot fetch distribution RDF — marking as having NO data services.")
      dataset_failed <- c(distribution_failed, distr_url)
      next
    }
    
    # Try to parse and add to RDF graph
    tryCatch(
      {
        g_all <- rdf_parse(
          doc    = turtle_txt,
          format = "turtle",
          rdf    = g_all
        )
      },
      error = function(e) {
        message("  Parse error: ", e$message)
        distribution_failed <- c(distribution_failed, distr_url)
        next
      }
    )
  }
  
  #14=========================================
  df_triples <- rdf_query(
    g_all,
    'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  ) %>% 
    distinct()
  
  #15==========================================
  #let's find data service URLs
  P_DCAT_DATASERVICE <- "http://www.w3.org/ns/dcat#accessService"
  
  
  distribution_to_dataservice <- df_triples %>%
    filter(p == P_DCAT_DATASERVICE) %>%
    transmute(
      distribution     = s,
      dataservice = o
    ) %>%
    distinct()
  
  dataservice_urls_raw <- unique(distribution_to_dataservice$dataservice)
  dataservice_urls <- normalize_url(dataservice_urls_raw)
  
  
  next_level_conenction <- list(
    
    fdp_to_catalog              = fdp_to_catalog,
    catalog_to_dataset          = catalog_to_dataset,
    dataset_to_distribution     = dataset_to_distribution, 
    distribution_to_dataservice = distribution_to_dataservice
  )
  
  return(list(
    graph  = g_all,
    connections = next_level_conenction
  ))
}