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
  
  fdp_catalog_summary <- fdp_to_catalog %>%
    distinct(fdp, catalog) %>%
    group_by(fdp) %>%
    summarise(
      n_catalogs = n(),
      catalogs = paste(sort(catalog), collapse = ", "),
      .groups = "drop"
    )
  
  #10 active FDPs, 7 have catalog
  
  LDP_CONTAINER <- "http://www.w3.org/ns/ldp#contains"
  
  catalog_containers <- df_triples %>%
    filter(p == LDP_CONTAINER) %>%
    transmute(
      catalog_container = s,
      catalog = o
    ) %>%
    distinct()
  
  #join FDPs and containers by catalogs
  all_catalogs_and_their_containers <- fdp_to_catalog %>%
    full_join(catalog_containers, by = "catalog") %>%
    arrange(catalog)
  
# save fdps for which catalogs that were created but not linked 
   catalogs_not_linked_to_FDP <- all_catalogs_and_their_containers %>%
    filter(is.na(fdp)) %>%
    mutate(
      fdp = sub("/catalog/?$", "", catalog_container)
    ) %>%
    distinct(fdp, catalog) %>%
    select(fdp, catalog)
  
 #==============semantic enrichment of FDP==================
   
  # SEMANTIC_THEME <-  "http://www.w3.org/ns/dcat#theme"
  # 
  # 
  # fdp_semantic_theme <- df_triples %>%
  #   filter(p == SEMANTIC_THEME) %>%
  #   transmute(
  #     fdp     = s,
  #     semantic_theme = o
  #   ) %>%
  #   distinct(fdp, semantic_theme)
  # 
  # #keyword
  # KEYWORD1 <-  	"http://www.w3.org/ns/dcat#keyword"
  # KEYWORD2 <-  "http://purl.org/dc/terms/keyword"
  # 
  # fdp_keywords <- df_triples %>%
  #   filter(p %in% c(KEYWORD1, KEYWORD2)) %>%
  #   transmute(
  #     fdp     = s,
  #     keywords = o
  #   ) %>%
  #   distinct(fdp, keywords)
  # 
  # #join keywords and theme by fdp
  # 
  # theme_keywords_by_fdp <- fdp_semantic_theme %>%
  #   full_join(fdp_keywords, by = "fdp", relationship = "many-to-many") %>%
  #   arrange(fdp) 
  
  remove_port_fdp <- function(x) {
    x <- sub("/+$", "", x)
    x <- sub(":7070$", "", x)
    x
  }
  

  fdp_summary <- tibble(
    fdp = unique(remove_port_fdp(fdp_list))
  ) %>%
    # left_join(
    #   fdp_keywords %>%
    #     distinct(fdp, keywords) %>%
    #     group_by(fdp) %>%
    #     summarise(
    #       n_keywords = n(),
    #       keywords = paste(sort(keywords), collapse = ", "),
    #       .groups = "drop"
    #     ),
    #   by = "fdp"
    # ) %>%
    # left_join(
    #   fdp_semantic_theme %>%
    #     distinct(fdp, semantic_theme) %>%
    #     group_by(fdp) %>%
    #     summarise(
    #       n_themes = n(),
    #       themes = paste(sort(semantic_theme), collapse = ", "),
    #       .groups = "drop"
    #     ),
    #   by = "fdp"
    # ) %>%
    left_join(
      fdp_to_catalog %>%
        distinct(fdp, catalog) %>%
        count(fdp, name = "n_catalogs"),
      by = "fdp"
    ) %>%
    left_join(
      fdp_to_catalog %>%
        distinct(fdp, catalog) %>%
        group_by(fdp) %>%
        summarise(
          catalogs = paste(sort(catalog), collapse = ", "),
          .groups = "drop"
        ),
      by = "fdp"
    ) %>%
    left_join(
      catalogs_not_linked_to_FDP %>%
        distinct(fdp, catalog) %>%
        count(fdp, name = "n_unlinked_catalogs"),
      by = "fdp"
    ) %>%
    mutate(
      # n_keywords = coalesce(n_keywords, 0L),
      # n_themes = coalesce(n_themes, 0L),
      n_catalogs = coalesce(n_catalogs, 0L),
      n_unlinked_catalogs = coalesce(n_unlinked_catalogs, 0L),
      #keywords = coalesce(keywords, ""),
      #themes = coalesce(themes, ""),
      catalogs = coalesce(catalogs, "")
    ) %>%
    arrange(fdp)
  
  #write.csv(fdp_summary,"fdp_summary1.csv", row.names = FALSE)


#24 catalogs
  catalog_urls_raw <- unique(fdp_to_catalog$catalog)
  catalog_urls <- normalize_url(catalog_urls_raw)
  #7==========================================
  
  catalog_failed <- character()
  all_resource_type_summary <- list()
  all_problem_resources <- list()
  all_resource_inventory <- list()
  #all_catalog_semantics <- list()
  #all_catalog_semantics_summary <- list()
  
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
    
    catalog_triples <- tryCatch(
      {
        parse_ttl_triples(turtle_txt)
      },
      error = function(e) {
        message("  Local parse error: ", e$message)
        catalog_failed <- c(catalog_failed, cat_url)
        return(NULL)
      }
    )
    
    if (is.null(catalog_triples)) {
      next
    }
    
    # catalog_semantics_result <- catalog_semantics_check(
    #   catalog_url = cat_url,
    #   catalog_triples = catalog_triples,
    #   envir = new.env()
    # )
    
    fdp_for_catalog <- fdp_to_catalog %>%
      filter(catalog == cat_url) %>%
      pull(fdp) %>%
      unique()
    
    if (length(fdp_for_catalog) == 0) {
      fdp_for_catalog <- NA_character_
    }
    
    catalog_check_result <- run_catalog_resource_container_check(
      catalog_url = cat_url,
      catalog_triples = catalog_triples,
      envir = new.env()
    )
    
    all_resource_type_summary[[length(all_resource_type_summary) + 1]] <-
      catalog_check_result$resource_type_summary %>%
      mutate(
        fdp = fdp_for_catalog[1],
        catalog_url = cat_url,
        .before = 1
      )
    
    all_problem_resources[[length(all_problem_resources) + 1]] <-
      catalog_check_result$problem_resources %>%
      mutate(
        fdp = fdp_for_catalog[1],
        .before = 1
      )
    
    all_resource_inventory[[length(all_resource_inventory) + 1]] <-
      catalog_check_result$resource_container_comparison %>%
      mutate(
        fdp = fdp_for_catalog[1],
        catalog_url = cat_url,
        .before = 1
      )
    
    # all_catalog_semantics[[length(all_catalog_semantics) + 1]] <-
    #   catalog_semantics_result$semantics_long %>%
    #   mutate(
    #     fdp = fdp_for_catalog[1],
    #     .before = 1
    #   )
    
    # all_catalog_semantics_summary[[length(all_catalog_semantics_summary) + 1]] <-
    #   catalog_semantics_result$semantics_summary %>%
    #   mutate(
    #     fdp = fdp_for_catalog[1],
    #     .before = 1
    #   )
    
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
  
  all_resource_type_summary_df <- bind_rows(all_resource_type_summary)
  all_problem_resources_df <- bind_rows(all_problem_resources)
  all_resource_inventory_df <- bind_rows(all_resource_inventory)
  # all_catalog_semantics_df <- bind_rows(all_catalog_semantics)
  # all_catalog_semantics_summary_df <- bind_rows(all_catalog_semantics_summary)
  
  # write.csv(all_resource_type_summary_df,"all_resource_type_summary_df1.csv", row.names = FALSE)
  # write.csv(all_problem_resources_df,"all_problem_resources_df1.csv", row.names = FALSE)
  # write.csv(all_resource_inventory_df,"all_resource_inventory_df1.csv", row.names = FALSE)
  # write.csv(all_catalog_semantics_df,"all_catalog_semantics_df1.csv", row.names = FALSE)
  # write.csv(all_catalog_semantics_summary_df,"all_catalog_semantics_summary_df1.csv", row.names = FALSE)
  

  
  catalog_tables <- all_resource_type_summary_df %>%
    group_by(fdp, catalog_url) %>%
    group_split()
  
  catalog_tables <- lapply(catalog_tables, function(df_cat) {
    total_resources <- sum(df_cat$n_container_resources, na.rm = TRUE)
    
    df_cat %>%
      select(
        fdp,
        catalog_url,
        resource_type,
        n_resources = n_container_resources
      ) %>%
      mutate(total_resources = total_resources, .before = resource_type)
  })
  
  names(catalog_tables) <- sapply(catalog_tables, function(x) unique(x$catalog_url))
  
  dataset_urls <- all_resource_inventory_df %>%
    filter(resource_type == "dataset") %>%
    pull(resource_url) %>%
    unique()
  
  patientregistry_urls <- all_resource_inventory_df %>%
    filter(resource_type %in% c("patientregistry", "patientRegistry")) %>%
    pull(resource_url) %>%
    unique()
  
  biobank_urls <- all_resource_inventory_df %>%
    filter(resource_type == "biobank") %>%
    pull(resource_url) %>%
    unique()
  
  dataservice_urls <- all_resource_inventory_df %>%
    filter(resource_type %in% c("dataService", "dataservice1", "dataservice2")) %>%
    pull(resource_url) %>%
    unique()
  
  resource_urls_by_catalog <- all_resource_inventory_df %>%
    select(fdp, catalog_url, resource_type, resource_url) %>%
    distinct() %>%
    arrange(fdp, catalog_url, resource_type, resource_url)
  
  
  
  # catalog_to_dataset <- df_triples %>%
  #   filter(p == P_DCAT_DATASET) %>%
  #   transmute(
  #     catalog = s,
  #     dataset = o
  #   ) %>%
  #   distinct()
  # 
  # 
  # dataset_containers <- df_triples %>%
  #   filter(
  #     p == LDP_CONTAINER,
  #     !s %in% catalog_containers$catalog_container
  #   ) %>%
  #   transmute(
  #     dataset_container = s,
  #     dataset = o
  #   ) %>%
  #   distinct()
  # 
  # #join catalogs and dataset containers by datasets
  # all_datasets_and_their_containers <- catalog_to_dataset %>%
  #   full_join(dataset_containers, by = "dataset") %>%
  #   arrange(dataset)
  # 
  # # save catalogs for which datasets that were created but not linked 
  # datasets_not_linked_to_FDP <- all_datasets_and_their_containers %>%
  #   filter(is.na(catalog)) %>%
  #   mutate(
  #     dataset_type = sub("^.*/([^/]+)/?$", "\\1", dataset_container),
  #     fdp = sub("/[^/]+/?$", "", dataset_container)
  #   ) %>%
  #   distinct(fdp, dataset_type, dataset) %>%
  #   select(fdp, dataset_type, dataset)
  
#Check which of the unlinked datasets cannot be accessed==============
  check_url_status <- function(url, timeout_seconds = 10) {
    out <- tryCatch(
      {
        res <- GET(
          url,
          timeout(timeout_seconds),
          add_headers(Accept = "text/turtle, application/ld+json;q=0.9, */*;q=0.1")
        )
        
        list(
          accessible = status_code(res) < 400,
          http_status = status_code(res),
          request_error = NA_character_
        )
      },
      error = function(e) {
        list(
          accessible = FALSE,
          http_status = NA_integer_,
          request_error = e$message
        )
      }
    )
    
    tibble(
      accessible = out$accessible,
      http_status = out$http_status,
      request_error = out$request_error
    )
  }
  
  # dataset_access_check <- datasets_not_linked_to_FDP %>%
  #   rowwise() %>%
  #   do(bind_cols(
  #     .,
  #     check_url_status(.$dataset)
  #   )) %>%
  #   ungroup()
  
  #statistics per fdp, how many datasets are inaccessible
  
  # dataset_status_by_fdp_wide <- dataset_access_check %>%
  #   mutate(
  #     status_label = ifelse(is.na(http_status), "request_error", paste0("status_", http_status))
  #   ) %>%
  #   count(fdp, dataset_type, status_label, name = "n_datasets") %>%
  #   pivot_wider(
  #     names_from = status_label,
  #     values_from = n_datasets,
  #     values_fill = 0
  #   ) %>%
  #   arrange(fdp, dataset_type)
  
#==============end of access test=====================================
  
  browser()
  
  # dataset_urls_raw <- unique(catalog_to_dataset$dataset)
  # 
  # dataset_urls <- normalize_url(dataset_urls_raw)
  #10=====================================
    #Only FOR Datasets
  
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
