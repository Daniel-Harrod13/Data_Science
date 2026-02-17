suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

source(file.path("R", "figures.R"))

plot_prop_bars_by_sex <- get("plot_prop_bars_by_sex", mode = "function")
plot_frequency_heatmap <- get("plot_frequency_heatmap", mode = "function")
plot_residual_heatmap <- get("plot_residual_heatmap", mode = "function")
save_mosaic_plot <- get("save_mosaic_plot", mode = "function")

check_required_packages <- function() {
  required <- c("ggplot2", "dplyr", "tidyr", "scales")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      paste(
        "Missing required package(s):",
        paste(missing, collapse = ", "),
        "\nInstall via install.packages(c(",
        paste(sprintf('"%s"', missing), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
}

cramers_v <- function(chisq_obj) {
  if (!inherits(chisq_obj, "htest")) {
    stop("cramers_v expects a chisq.test result.", call. = FALSE)
  }

  obs <- chisq_obj$observed
  n <- sum(obs)
  r <- nrow(obs)
  k <- ncol(obs)

  sqrt(unname(chisq_obj$statistic) / (n * min(r - 1, k - 1)))
}

analyze_hair_eye <- function(project_root = ".") {
  check_required_packages()

  data_dir <- file.path(project_root, "data", "derived")
  figures_dir <- file.path(project_root, "figures")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  data_long <- as.data.frame(datasets::HairEyeColor) |>
    dplyr::mutate(
      Hair = factor(.data$Hair, levels = c("Black", "Brown", "Red", "Blond")),
      Eye = factor(.data$Eye, levels = c("Brown", "Blue", "Hazel", "Green")),
      Sex = factor(.data$Sex, levels = c("Male", "Female"))
    )

  overall_tbl <- xtabs(Freq ~ Hair + Eye, data = data_long)
  male_tbl <- xtabs(Freq ~ Hair + Eye, data = dplyr::filter(data_long, .data$Sex == "Male"))
  female_tbl <- xtabs(Freq ~ Hair + Eye, data = dplyr::filter(data_long, .data$Sex == "Female"))

  chisq_overall <- suppressWarnings(chisq.test(overall_tbl))
  chisq_male <- suppressWarnings(chisq.test(male_tbl))
  chisq_female <- suppressWarnings(chisq.test(female_tbl))

  test_summary <- dplyr::tibble(
    model = c("Overall", "Male", "Female"),
    chi_square = c(
      unname(chisq_overall$statistic),
      unname(chisq_male$statistic),
      unname(chisq_female$statistic)
    ),
    df = c(
      unname(chisq_overall$parameter),
      unname(chisq_male$parameter),
      unname(chisq_female$parameter)
    ),
    p_value = c(chisq_overall$p.value, chisq_male$p.value, chisq_female$p.value),
    cramers_v = c(cramers_v(chisq_overall), cramers_v(chisq_male), cramers_v(chisq_female))
  )

  residual_long <- bind_rows(
    as.data.frame(as.table(chisq_male$stdres)) |> dplyr::mutate(Sex = "Male"),
    as.data.frame(as.table(chisq_female$stdres)) |> dplyr::mutate(Sex = "Female")
  ) |>
    dplyr::rename(StdResid = "Freq") |>
    dplyr::mutate(
      Hair = factor(.data$Hair, levels = levels(data_long$Hair)),
      Eye = factor(.data$Eye, levels = levels(data_long$Eye)),
      Sex = factor(.data$Sex, levels = levels(data_long$Sex))
    )

  p_prop <- plot_prop_bars_by_sex(data_long)
  p_heat <- plot_frequency_heatmap(data_long)
  p_resid <- plot_residual_heatmap(residual_long)

  ggplot2::ggsave(
    filename = file.path(figures_dir, "fig_01_prop_bars_by_sex.png"),
    plot = p_prop,
    width = 10,
    height = 6,
    dpi = 320
  )

  ggplot2::ggsave(
    filename = file.path(figures_dir, "fig_02_frequency_heatmap.png"),
    plot = p_heat,
    width = 8,
    height = 6,
    dpi = 320
  )

  ggplot2::ggsave(
    filename = file.path(figures_dir, "fig_03_residual_heatmap.png"),
    plot = p_resid,
    width = 10,
    height = 6,
    dpi = 320
  )

  save_mosaic_plot(
    contingency_table = overall_tbl,
    output_path = file.path(figures_dir, "fig_04_mosaic_overall.png")
  )

  write.csv(data_long, file.path(data_dir, "hair_eye_long.csv"), row.names = FALSE)
  write.csv(test_summary, file.path(data_dir, "chi_square_results.csv"), row.names = FALSE)
  write.csv(residual_long, file.path(data_dir, "standardized_residuals_by_sex.csv"), row.names = FALSE)

  invisible(
    list(
      data_long = data_long,
      overall_table = overall_tbl,
      male_table = male_tbl,
      female_table = female_tbl,
      chisq_overall = chisq_overall,
      chisq_male = chisq_male,
      chisq_female = chisq_female,
      test_summary = test_summary,
      residual_long = residual_long,
      plots = list(
        proportion_bars = p_prop,
        frequency_heatmap = p_heat,
        residual_heatmap = p_resid
      )
    )
  )
}

if (identical(environment(), globalenv()) && !length(grep("^source\\(", commandArgs()))) {
  message("Running HairEyeColor analysis pipeline...")
  analyze_hair_eye(project_root = ".")
  message("Done. Outputs saved in data/derived and figures/.")
}
