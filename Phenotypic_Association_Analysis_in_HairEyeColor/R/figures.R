# nolint start: object_usage_linter
utils::globalVariables(".data")

academic_theme <- function(base_size = 12, base_family = "Helvetica") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(size = base_size),
      plot.caption = ggplot2::element_text(size = base_size - 2, color = "grey30"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

hair_palette <- c(
  "Black" = "#1b1b1b",
  "Brown" = "#8c510a",
  "Red" = "#d95f02",
  "Blond" = "#fdb863"
)

eye_palette <- c(
  "Brown" = "#7f3b08",
  "Blue" = "#2b8cbe",
  "Hazel" = "#8073ac",
  "Green" = "#1b9e77"
)

residual_palette <- c(
  "Low" = "#2b8cbe",
  "Neutral" = "#f7f7f7",
  "High" = "#d7301f"
)

plot_prop_bars_by_sex <- function(data_long) {
  plot_data <- data_long |>
    dplyr::group_by(.data$Sex, .data$Hair) |>
    dplyr::mutate(prop_within_hair = .data$Freq / sum(.data$Freq)) |>
    dplyr::ungroup()

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Hair, y = .data$prop_within_hair, fill = .data$Eye)) +
    ggplot2::geom_col(position = "fill", color = "white", linewidth = 0.25) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Sex)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = eye_palette, drop = FALSE) +
    ggplot2::labs(
      title = "Distribution of Eye Color Within Hair Color Groups",
      subtitle = "Faceted by sex; bars sum to 100% within each hair color",
      x = "Hair Color",
      y = "Proportion",
      fill = "Eye Color",
      caption = "Data source: datasets::HairEyeColor"
    ) +
    academic_theme()
}

plot_frequency_heatmap <- function(data_long) {
  agg <- data_long |>
    dplyr::group_by(.data$Hair, .data$Eye) |>
    dplyr::summarise(Freq = sum(.data$Freq), .groups = "drop")

  ggplot2::ggplot(agg, ggplot2::aes(x = .data$Eye, y = .data$Hair, fill = .data$Freq)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), size = 3.8, color = "black") +
    ggplot2::scale_fill_gradient(low = "#f0f9e8", high = "#084081") +
    ggplot2::labs(
      title = "Observed Counts by Hair and Eye Color",
      subtitle = "Aggregated across sex",
      x = "Eye Color",
      y = "Hair Color",
      fill = "Count",
      caption = "Data source: datasets::HairEyeColor"
    ) +
    academic_theme()
}

plot_residual_heatmap <- function(residual_long) {
  ggplot2::ggplot(residual_long, ggplot2::aes(x = .data$Eye, y = .data$Hair, fill = .data$StdResid)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$StdResid)), size = 3.5) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Sex)) +
    ggplot2::scale_fill_gradient2(
      low = "#2b8cbe",
      mid = "#f7f7f7",
      high = "#d7301f",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Standardized Residuals from Independence Models",
      subtitle = "Positive values indicate over-representation; negative values indicate under-representation",
      x = "Eye Color",
      y = "Hair Color",
      fill = "Std. Residual",
      caption = "Rule of thumb: absolute residual > 2 suggests notable deviation"
    ) +
    academic_theme()
}

save_mosaic_plot <- function(contingency_table, output_path) {
  grDevices::png(filename = output_path, width = 1800, height = 1200, res = 300)
  graphics::mosaicplot(
    contingency_table,
    color = TRUE,
    las = 1,
    main = "Mosaic Plot of Hair and Eye Color",
    xlab = "Eye Color",
    ylab = "Hair Color",
    cex.axis = 0.9
  )
  grDevices::dev.off()
}
# nolint end
