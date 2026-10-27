# Plot successful metadata-processing counts, with active/inactive stacked separately.
plot_table1 <- function(table1_active = NULL, table1_inactive = NULL, groups = NULL, layers = NULL) {
  resource_types <- c("Datasets", "Biobanks", "Patient registries", "Guidelines", "Data Services")
  categories <- c("FDPs", "Catalogs", paste0("Resources - ", resource_types),
                  "Distributions", "DataServices")
  families <- c("FDP", "Catalog", rep("Resource", length(resource_types)),
                "Distribution", "Data Service")
  if (!is.null(layers)) {
    keep <- categories %in% layers
    categories <- categories[keep]
    families <- families[keep]
    extra_layers <- setdiff(layers, categories)
    categories <- c(categories, extra_layers)
    families <- c(families, rep("Resource", length(extra_layers)))
  }
  format_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

  prepare_counts <- function(table, status) {
    totals <- setNames(numeric(length(categories)), categories)
    if (!is.null(table)) {
      if (!all(c("Metadata Layer", "Fetch successful") %in% names(table))) {
        stop(status, " table must contain Metadata Layer and Fetch successful columns.")
      }
      if (anyNA(table[["Metadata Layer"]]) || any(!table[["Metadata Layer"]] %in% categories)) {
        stop(status, " table contains unrecognised metadata layers; rerun the updated crawler.")
      }
      counts <- table[["Fetch successful"]]
      if (!is.numeric(counts) || anyNA(counts) || any(!is.finite(counts) | counts < 0)) {
        stop(status, " Fetch successful must contain finite, non-negative counts.")
      }
      for (category in unique(table[["Metadata Layer"]])) {
        totals[category] <- sum(counts[table[["Metadata Layer"]] == category])
      }
    }
    data.frame(layer = factor(categories, levels = categories),
      count = unname(totals), status = status, family = families)
  }

  if (is.null(groups)) groups <- list(ACTIVE = table1_active, INACTIVE = table1_inactive)
  plot_data <- do.call(rbind, lapply(names(groups), function(state) prepare_counts(groups[[state]], state)))
  plot_data$status <- factor(plot_data$status, levels = names(groups))
  palette <- c(
    "ACTIVE FDP" = "#6A9DE8", "ACTIVE Catalog" = "#083680",
    "ACTIVE Resource" = "#a1c2f0", "ACTIVE Distribution" = "#ad7b23",
    "ACTIVE Data Service" = "#E67E22",
    "INACTIVE FDP" = "#99abc7", "INACTIVE Catalog" = "#4c6185",
    "INACTIVE Resource" = "#c3d3eb", "INACTIVE Distribution" = "#ab9878",
    "INACTIVE Data Service" = "#e3b388")
  extras <- setdiff(names(groups), c("ACTIVE", "INACTIVE"))
  for (state in extras) {
    palette[paste(state, unique(families))] <- grDevices::hcl.colors(length(unique(families)), "Dark 3")
  }
  plot_data$segment <- factor(paste(plot_data$status, plot_data$family), levels = names(palette))
  plot_data$label <- format_count(plot_data$count)
  plot_data$label_colour <- ifelse(plot_data$family == "Catalog", "white", "#182536")
  # Active is the lower segment; labels use the same order as the stacked bars.
  plot_data$label_y <- ave(plot_data$count, plot_data$layer, FUN = function(x) cumsum(x) - x / 2)
  bar_totals <- data.frame(layer = factor(categories, levels = categories),
    count = vapply(categories, function(layer) sum(plot_data$count[plot_data$layer == layer]), numeric(1)))
  bar_totals$label <- format_count(bar_totals$count)
  axis_labels <- sub("Resources - ", "Resources\n", categories, fixed = TRUE)

  ggplot2::ggplot(plot_data,
    ggplot2::aes(x = layer, y = count, fill = segment, group = status)) +
    ggplot2::geom_col(width = 0.68, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(data = plot_data[plot_data$count > 0, ],
      ggplot2::aes(y = label_y, label = label, colour = label_colour),
      fontface = "bold", size = 3.3, check_overlap = TRUE, show.legend = FALSE) +
    ggplot2::geom_text(data = bar_totals,
      ggplot2::aes(x = layer, y = count, label = label), inherit.aes = FALSE,
      vjust = -0.6, fontface = "bold", size = 3.8) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_manual(values = palette, limits = names(palette), drop = FALSE, name = NULL) +
    ggplot2::scale_x_discrete(labels = setNames(axis_labels, categories), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = format_count,
      expand = ggplot2::expansion(mult = c(0, 0.12), add = c(0, 1))) +
    ggplot2::labs(title = "Metadata documents by layer",
      subtitle = "Successfully fetched and parsed Turtle, split by active and inactive FDPs",
      x = "Metadata layer", y = "Successful count",
      caption = "Resources - Data Services are catalog-linked. DataServices follows distributions.") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(colour = "#263238"),
      axis.text.x = ggplot2::element_text(size = 10, lineheight = 1.05),
      axis.ticks.x = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#566573"),
      plot.margin = ggplot2::margin(12, 16, 12, 12)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
}
