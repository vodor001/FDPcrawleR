# Full resource tree. A conserved unit is one distinct discovered terminal URL
# path, including incomplete paths. FDPs can contribute multiple paths/types.
fdp_path_sankey_layout <- function(active_paths = NULL, inactive_paths = NULL, groups = NULL, dcat_only = FALSE) {
  identity <- c("fdp", "catalog_url", "resource_url", "distribution", "dataservice")
  required <- c(identity, "resource_layer", "fdp_status", "catalog_status",
                "resource_status", "distribution_status", "path_status")
  prepare <- function(paths, state) {
    if (is.null(paths) || !all(required %in% names(paths))) {
      stop("The full Sankey requires path_details from both crawl results. Save updated crawl results first.")
    }
    paths <- as.data.frame(paths, stringsAsFactors = FALSE)
    if (!"projection_boundary" %in% names(paths)) paths$projection_boundary <- rep(NA_character_, nrow(paths))
    paths <- paths[!duplicated(paths[identity]), c(required, "projection_boundary"), drop = FALSE]
    paths$state <- rep(state, nrow(paths))
    paths
  }
  if (is.null(groups)) groups <- list(ACTIVE = active_paths, INACTIVE = inactive_paths)
  paths <- do.call(rbind, lapply(names(groups), function(state) prepare(groups[[state]], state)))
  if (is.null(paths) || !nrow(paths)) return(list(nodes = data.frame(), links = data.frame(), paths = paths, height = 500))
  labels_order <- c("All paths", "ACTIVE", "INACTIVE", "FDP unavailable", "No catalog", "Catalog found",
    "Catalog unavailable", "No resources", "Datasets", "Patient registries", "Biobanks",
    "Data services", "Guidelines", "Other resources", "Resource unavailable", "Not traversed",
    "No distribution", "Distribution found", "Distribution unavailable", "No data service", "Data service found", "Outside standard route", "Outside DCAT routes")
  types <- c(Dataset = "Datasets", PatientRegistry = "Patient registries", Biobank = "Biobanks",
             DataService = "Data services", Guideline = "Guidelines")
  if (dcat_only) types <- types[c("Dataset", "DataService")]
  present <- function(value) !is.na(value) && nzchar(value)
  ok <- function(value) !is.na(value) && value == "ok"
  sequences <- lapply(seq_len(nrow(paths)), function(i) {
    row <- paths[i, ]
    labels <- c("All paths", row$state)
    if (!ok(row$fdp_status)) return(c(labels, "FDP unavailable"))
    if (row$projection_boundary %in% "fdp") return(c(labels, "Outside standard route"))
    if (!present(row$catalog_url)) return(c(labels, "No catalog"))
    labels <- c(labels, "Catalog found")
    if (!ok(row$catalog_status)) return(c(labels, "Catalog unavailable"))
    if (row$projection_boundary %in% "catalog_url") return(c(labels, "Outside standard route"))
    if (!present(row$resource_url)) return(c(labels, "No resources"))
    type <- unname(types[row$resource_layer])
    if (dcat_only && is.na(type)) return(c(labels,
      if (!is.na(row$resource_status) && !ok(row$resource_status)) "Resource unavailable" else "Outside DCAT routes"))
    if (is.na(type)) type <- "Other resources"
    labels <- c(labels, type)
    if (row$projection_boundary %in% "resource_url") return(c(labels, "Outside standard route"))
    # A service linked directly from a catalog terminates at the resource stage.
    if (type == "Data services") return(labels)
    if (is.na(row$resource_status) || row$path_status == "not_traversed") return(c(labels, "Not traversed"))
    if (!ok(row$resource_status)) return(c(labels, "Resource unavailable"))
    if (!present(row$distribution)) return(c(labels, if (present(row$dataservice)) "Data service found" else "No distribution"))
    labels <- c(labels, "Distribution found")
    if (!ok(row$distribution_status)) return(c(labels, "Distribution unavailable"))
    if (row$projection_boundary %in% c("distribution", "dataservice")) return(c(labels, "Outside standard route"))
    c(labels, if (present(row$dataservice)) "Data service found" else "No data service")
  })
  # Separate nodes for each prefix preserve ownership and prevent merging
  # resource lanes at the distribution/service stages (which causes crossings).
  records <- list()
  for (i in seq_along(sequences)) {
    sequence <- sequences[[i]]
    for (j in seq_along(sequence)) {
      key <- paste(sequence[seq_len(j)], collapse = "|")
      if (is.null(records[[key]])) records[[key]] <- list(
        label = sequence[[j]], depth = j, parent = if (j == 1L) NA_character_ else
          paste(sequence[seq_len(j - 1L)], collapse = "|"), rows = integer())
      records[[key]]$rows <- c(records[[key]]$rows, i)
    }
  }
  if (!length(records)) return(list(nodes = data.frame(), links = data.frame(), paths = paths, height = 500))
  # Depth-first order gives each parent a contiguous span of terminal leaves.
  ordered <- character()
  visit <- function(key) {
    ordered <<- c(ordered, key)
    children <- names(records)[vapply(records, function(x) identical(x$parent, key), logical(1))]
    children <- children[order(vapply(records[children], function(x) match(x$label, labels_order), integer(1)))]
    for (child in children) visit(child)
  }
  visit("All paths")
  nodes <- do.call(rbind, lapply(ordered, function(key) {
    item <- records[[key]]
    rows <- paths[item$rows, , drop = FALSE]
    data.frame(key = key, label = item$label, depth = item$depth, parent = item$parent,
      value = nrow(rows), n_fdps = length(unique(rows$fdp)),
      incomplete = sum(!rows$path_status %in% c("complete", "outside_standard_route")),
      outside = sum(!is.na(rows$projection_boundary)), stringsAsFactors = FALSE)
  }))
  nodes$id <- seq_len(nrow(nodes)) - 1L
  nodes$x <- c(.015, .18, .38, .58, .78, .98)[nodes$depth]
  # The direct Dataset -> DataService route skips the Distribution column.
  nodes$x[nodes$label == "Data service found"] <- .98
  nodes$y <- .5
  leaves <- which(!nodes$key %in% nodes$parent)
  pad <- 44
  height <- max(1000, 910 + pad * (length(leaves) - 1L))
  plot_height <- height - 110
  scale <- (plot_height - pad * (length(leaves) - 1L)) / nrow(paths)
  leaf_heights <- nodes$value[leaves] * scale
  starts <- c(0, head(cumsum(leaf_heights + pad), -1L))
  ends <- starts + leaf_heights
  for (i in seq_len(nrow(nodes))) {
    descendants <- which(nodes$key[leaves] == nodes$key[i] |
      startsWith(nodes$key[leaves], paste0(nodes$key[i], "|")))
    nodes$y[i] <- (min(starts[descendants]) + max(ends[descendants])) / (2 * plot_height)
  }
  palette <- c("Datasets" = "#6A9DE8", "Patient registries" = "#8872B3",
    "Biobanks" = "#4D997A", "Data services" = "#E67E22", "Guidelines" = "#359BA5",
    "Other resources" = "#A8936B")
  nodes$color <- vapply(strsplit(nodes$key, "|", fixed = TRUE), function(parts) {
    label <- tail(parts, 1)
    if (label == "All paths") return("#10264a")
    if (label == "ACTIVE") return("#6A9DE8")
    if (label == "INACTIVE") return("#99abc7")
    if (label == "Catalog found") return("#304F78")
    if (grepl("^No |unavailable|Not traversed", label)) return("#A4AAB2")
    type <- parts[parts %in% names(palette)]
    if (length(type)) unname(palette[type[[1]]]) else "#99abc7"
  }, character(1))
  nodes$hover <- paste0(gsub("|", " / ", nodes$key, fixed = TRUE),
    "<br>", nodes$value, " discovered paths", "<br>", nodes$n_fdps, " distinct FDPs",
    "<br>", nodes$incomplete, " paths with incomplete standard-route traversal",
    "<br>", nodes$outside, " paths with links outside this projection; see traversal evidence")
  children <- which(!is.na(nodes$parent))
  links <- data.frame(source = match(nodes$parent[children], nodes$key) - 1L,
    target = nodes$id[children], value = nodes$value[children])
  list(nodes = nodes, links = links, paths = paths, pad = pad, height = height)
}

plot_fdp_paths <- function(active_paths = NULL, inactive_paths = NULL, groups = NULL, dcat_only = FALSE) {
  data <- fdp_path_sankey_layout(active_paths, inactive_paths, groups = groups, dcat_only = dcat_only)
  nodes <- data$nodes
  if (!nrow(nodes)) {
    chart <- plotly::layout(plotly::plot_ly(height = 500), annotations = list(list(
      text = "No discovered metadata paths", x = .5, y = .5, showarrow = FALSE)))
  } else {
    links <- data$links
    rgb <- grDevices::col2rgb(nodes$color[links$target + 1L])
    headers <- c("FDP paths", "Index state", "Catalog", "Resource", "Distribution", "Data service")
    if (dcat_only) headers[4] <- "Dataset / DataService"
    positions <- c(.015, .18, .38, .58, .78, .98)
    annotations <- lapply(seq_along(headers), function(i) list(text = paste0("<b>", headers[i], "</b>"),
      x = positions[i], y = 1 + 45 / (data$height - 110), xref = "paper", yref = "paper",
      xanchor = if (i == 1L) "left" else if (i == 6L) "right" else "center",
      showarrow = FALSE, font = list(size = 12, color = "#10264a")))
    labels <- gsub(" ", "<br>", nodes$label, fixed = TRUE)
    chart <- plotly::plot_ly(type = "sankey", orientation = "h", arrangement = "fixed", height = data$height,
      textfont = list(size = 11, color = "#26384b"), valueformat = ".0f",
      node = list(label = paste0(labels, "<br>", nodes$value, " paths"),
        customdata = nodes$hover, hovertemplate = "%{customdata}<extra></extra>",
        x = nodes$x, y = nodes$y, color = nodes$color, pad = data$pad,
        thickness = 16, line = list(color = "white", width = .7)),
      link = list(source = links$source, target = links$target, value = links$value,
        color = sprintf("rgba(%s,%s,%s,0.30)", rgb[1, ], rgb[2, ], rgb[3, ]),
        customdata = nodes$hover[links$target + 1L], hovertemplate = "%{customdata}<extra></extra>"))
    chart <- plotly::layout(chart, margin = list(l = 15, r = 15, t = 80, b = 30),
      font = list(family = "Arial", color = "#26384b"), annotations = annotations,
      paper_bgcolor = "white", plot_bgcolor = "white")
  }
  attr(chart, "sankey_height") <- data$height
  chart
}

