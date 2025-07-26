# R/plot_bullseye.R

#' Generate coordinates for bull's eye plot segments
#' @keywords internal
#' @noRd
get_segment_coords <- function(model = 17) {
  if (!model %in% c(16, 17)) {
    stop("Model must be 16 or 17.", call. = FALSE)
  }
  create_segment <- function(seg_num, seg_name, r_in, r_out, theta_start, theta_end, n = 20) {
    theta <- seq(theta_start, theta_end, length.out = n)
    x_outer <- r_out * cos(theta); y_outer <- r_out * sin(theta)
    x_inner <- r_in * cos(theta); y_inner <- r_in * sin(theta)
    dplyr::tibble(
      segment = seg_num, segment_name = seg_name,
      x = c(x_outer, rev(x_inner)), y = c(y_outer, rev(y_inner))
    )
  }
  angle_shift <- pi / 3
  angles <- seq(0, 2 * pi, length.out = 7) + angle_shift
  r1 <- 1; r2 <- 2; r3 <- 3
  basal_segments <- purrr::map_dfr(1:6, ~create_segment(seg_num=.x, seg_name=c("Basal Anterior", "Basal Anteroseptal", "Basal Inferoseptal", "Basal Inferior", "Basal Inferolateral", "Basal Anterolateral")[.x], r_in=r2, r_out=r3, theta_start=angles[.x], theta_end=angles[.x+1]))
  mid_segments <- purrr::map_dfr(1:6, ~create_segment(seg_num=.x+6, seg_name=c("Mid Anterior", "Mid Anteroseptal", "Mid Inferoseptal", "Mid Inferior", "Mid Inferolateral", "Mid Anterolateral")[.x], r_in=r1, r_out=r2, theta_start=angles[.x], theta_end=angles[.x+1]))
  apical_angles <- seq(0, 2*pi, length.out=5) + (pi/4)
  apical_segments <- purrr::map_dfr(1:4, ~create_segment(seg_num=.x+12, seg_name=c("Apical Anterior", "Apical Septal", "Apical Inferior", "Apical Lateral")[.x], r_in=0, r_out=r1, theta_start=apical_angles[.x], theta_end=apical_angles[.x+1]))
  all_segments <- dplyr::bind_rows(basal_segments, mid_segments, apical_segments)
  if (model == 17) {
    theta_apex <- seq(0, 2*pi, length.out=50)
    apex <- dplyr::tibble(segment=17, segment_name="Apex", x=0.4*cos(theta_apex), y=0.4*sin(theta_apex))
    apical_segments_17 <- purrr::map_dfr(1:4, ~create_segment(seg_num=.x+12, seg_name=c("Apical Anterior", "Apical Septal", "Apical Inferior", "Apical Lateral")[.x], r_in=0.4, r_out=r1, theta_start=apical_angles[.x], theta_end=apical_angles[.x+1]))
    all_segments <- dplyr::bind_rows(basal_segments, mid_segments, apical_segments_17, apex)
  }
  return(all_segments)
}


#' Create a Bull's Eye Plot from a Data Frame
#'
#' This is the main plotting engine for the beheart package. It creates a
#' bull's eye plot by calculating summary statistics from a data frame. It supports
#' extensive customization, faceting for group comparisons, and direct export.
#'
#' @param df The input data frame.
#' @param model An integer, 16 or 17, for the segmentation model.
#' @param group_col The unquoted name of the column identifying the segments.
#' @param value_col The unquoted name of the column containing the values to plot.
#' @param summary_fun The summary statistic to display: "mean_sd" or "median_iqr".
#' @param facet_by Optional unquoted name of a column to create side-by-side plots.
#' @param facet_labels Optional named vector for custom facet labels.
#' @param text_display What to display in segments: "value", "name", or "none".
#' @param palette A built-in palette ("default", "echo", "colorblind").
#' @param value_name A string for the legend's title. If NULL, a default is used.
#' @param show_anatomical_labels A boolean; if TRUE, labels for "Anterior", etc. are shown.
#' @param line_color The color of the lines separating segments.
#' @param font_family The font family for all text.
#' @param font_color_text The color of the text inside segments.
#' @param font_size_text The font size (in points) of the text inside segments.
#' @param font_color_labels The color of the peripheral anatomical labels.
#' @param font_size_labels The font size of the peripheral anatomical labels.
#' @param filename Optional path to save the plot (e.g., "my_plot.png").
#' @param width The width of the saved plot. Defaults to 7.
#' @param height The height of the saved plot. Defaults to 7.
#' @param units The units for width and height ("in", "cm", "mm"). Defaults to "in".
#' @param dpi The resolution for saved raster images. Defaults to 300.
#' @param stacked_text_spacing A numeric value controlling the vertical spacing of stacked text. Smaller values are tighter. Defaults to 0.2.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' # --- Create a sample long-format data frame ---
#' # This is the format required by the function. Use `prepare_bullseye_data`
#' # if your data is in a "wide" format.
#' set.seed(123)
#' long_dataset <- data.frame(
#'   segment = rep(1:17, each = 20),
#'   strain = rnorm(340, mean = -18, sd = 5),
#'   hypertension = rep(c("Yes", "No"), each = 10, times = 17)
#' )
#' long_dataset$strain[long_dataset$hypertension == "Yes"] <-
#'   long_dataset$strain[long_dataset$hypertension == "Yes"] + 3
#'
#' # --- Example 1: A simple, default plot ---
#' plot_bullseye_from_df(
#'   df = long_dataset,
#'   group_col = segment,
#'   value_col = strain
#' )
#'
#' # --- Example 2: Faceted plot with custom labels and export ---
#' \dontrun{
#' plot_bullseye_from_df(
#'   df = long_dataset,
#'   group_col = segment,
#'   value_col = strain,
#'   facet_by = hypertension,
#'   facet_labels = c("Yes" = "Hypertensive", "No" = "Normotensive"),
#'   palette = "echo",
#'   value_name = "Peak Strain (%)",
#'   filename = "hypertension_comparison.png",
#'   width = 11, height = 6
#' )
#' }
#'
#' # --- Example 3: Displaying segment names instead of values ---
#' plot_bullseye_from_df(
#'   df = long_dataset,
#'   group_col = segment,
#'   value_col = strain,
#'   text_display = "name"
#' )
plot_bullseye_from_df <- function(df, model = 17, group_col, value_col,
                                  summary_fun = "mean_sd", facet_by = NULL,
                                  facet_labels = NULL, text_display = "value",
                                  palette = "default", value_name = NULL,
                                  show_anatomical_labels = TRUE, line_color = "white",
                                  font_family = "sans", font_color_text = "black",
                                  font_size_text = 7.5, font_color_labels = "black",
                                  font_size_labels = 4, filename = NULL, width = 14,
                                  height = 8, units = "in", dpi = 600) {

  # --- 1. Data Summarization ---
  group_col_quo <- rlang::enquo(group_col)
  facet_by_quo <- rlang::enquo(facet_by)
  grouping_vars <- rlang::quos(!!group_col_quo, !!facet_by_quo)

  if (summary_fun == "mean_sd") {
    summary_df <- df |>
      dplyr::group_by(!!!grouping_vars) |>
      dplyr::summarize(plot_value = mean(!!rlang::enquo(value_col), na.rm = TRUE),
                       sd_val = sd(!!rlang::enquo(value_col), na.rm = TRUE), .groups = "drop") |>
      # This mutate is now separate and only sees the correct columns
      dplyr::mutate(
        display_text = dplyr::if_else(!!group_col_quo %in% c(14, 16, 17),
                                      sprintf(paste0("<b style='color:", font_color_text, "; font-size:", font_size_text, "pt; line-height:0.2em;'>%s<br>\u00b1<br>%s</b>"), round(plot_value, 1), round(sd_val, 1)),
                                      sprintf(paste0("<b style='color:", font_color_text, "; font-size:", font_size_text, "pt;'>%s \u00b1 %s</b>"), round(plot_value, 1), round(sd_val, 1))
        )
      )
    legend_title <- value_name %||% "Mean"
  } else { # median_iqr
    summary_df <- df |>
      dplyr::group_by(!!!grouping_vars) |>
      dplyr::summarize(plot_value = median(!!rlang::enquo(value_col), na.rm = TRUE),
                       q1 = quantile(!!rlang::enquo(value_col), 0.25, na.rm = TRUE),
                       q3 = quantile(!!rlang::enquo(value_col), 0.75, na.rm = TRUE), .groups = "drop") |>
      # This mutate is now separate and only sees the correct columns
      dplyr::mutate(
        display_text = dplyr::if_else(!!group_col_quo %in% c(14, 16, 17),
                                      sprintf(paste0("<b style='color:", font_color_text, "; font-size:", font_size_text, "pt; line-height:0.2em;'>%s<br>(%s - %s)</b>"), round(plot_value, 1), round(q1, 1), round(q3, 1)),
                                      sprintf(paste0("<b style='color:", font_color_text, "; font-size:", font_size_text, "pt;'>%s (%s - %s)</b>"), round(plot_value, 1), round(q1, 1), round(q3, 1))
        )
      )
    legend_title <- value_name %||% "Median"
  }

  # --- 2. Handle Facet Labels ---
  if (!rlang::quo_is_null(facet_by_quo)) {
    facet_col_name <- rlang::as_name(facet_by_quo)
    if (!is.null(facet_labels)) {
      summary_df[[facet_col_name]] <- factor(summary_df[[facet_col_name]],
                                             levels = names(facet_labels),
                                             labels = facet_labels)
    }
  }

  # --- 3. Prepare for Plotting ---
  coords <- get_segment_coords(model = model)
  join_by_vec <- "segment"; names(join_by_vec) <- rlang::as_name(group_col_quo)
  plot_data <- dplyr::left_join(summary_df, coords, by = join_by_vec)

  # Define palettes and bins
  breaks <- c(-Inf, -15, -10, -5, 0, 5, 10, 15, Inf)
  labels <- c("<= -15", "-14.9 to -10", "-9.9 to -5", "-4.9 to -0.0", "0.0 to 4.9", "5.0 to 9.9", "10.0 to 14.9", ">= 15")
  if (length(palette) == 1 && palette == "echo") {
    colors <- c("#0101fe", "#3a3cff", "#7977ff", "#b4b4fc", "#feb2b8", "#fd7678", "#fe4546", "#f8120e")
  } else if (length(palette) == 1 && palette == "default") {
    colors <- c("#FF0000", "#FF4500", "#FFA500", "#FFD700", "#FFFF00", "#9ACD32", "#32CD32", "#006400")
  } else {
    colors <- viridisLite::viridis(8)
  }
  colors <- stats::setNames(rev(colors), labels)
  plot_data$value_binned <- factor(
    cut(plot_data$plot_value, breaks = breaks, labels = names(colors), right = FALSE, include.lowest = TRUE),
    levels = names(colors)
  )

  # --- 4. Create Plot ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, group = segment, fill = value_binned)) +
    ggplot2::geom_polygon(color = line_color, linewidth = 1.2, show.legend = TRUE) +
    ggplot2::coord_equal(xlim = c(-4, 4), ylim = c(-4, 4)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      text = ggplot2::element_text(family = font_family)
    ) +
    ggplot2::scale_fill_manual(name = legend_title, values = colors, drop = FALSE, limits = names(colors), na.value = "grey80")

  # --- 5. Add Text Layers ---
  if (text_display != "none") {
    if (text_display == "name") {
      text_data <- plot_data |> dplyr::mutate(display_text = paste0("<b style='color:", font_color_text, "; font-size:7pt;'>", gsub(" ", "<br>", segment_name), "</b>"))
    } else { # "value"
      text_data <- plot_data # display_text is already correct from the summary step
    }
    label_pos_data <- text_data |>
      dplyr::group_by(!!!grouping_vars, display_text) |>
      dplyr::summarize(x = mean(x), y = mean(y), .groups = "drop") |>
      dplyr::mutate(
        scaling_factor = dplyr::case_when(
          # For 17-segment model, adjust segments 14-17 positioning
          model == 17 & !!group_col_quo %in% c(14, 16) ~ 1.1,  # Reduced from 1.35/1.4 to move closer to center
          model == 17 & !!group_col_quo == 17 ~ 1.0,               # Keep apex the same

          # For 16-segment model (or other models), keep original positioning
          model != 17 & !!group_col_quo %in% c(13,15) ~ 1.25,      # Larger shift for apical segments
          model != 17 & !!group_col_quo %in% c(14,16) ~ 1.25,
          model != 17 & !!group_col_quo == 17 ~ 1.0,               # No shift for apex

          TRUE ~ 1.05                                              # Default shift for all others
        ),
        x = x * scaling_factor,
        y = y * scaling_factor
      )

    p <- p + ggtext::geom_richtext(data = label_pos_data, ggplot2::aes(x = x, y = y, label = display_text), inherit.aes = FALSE, label.color = NA, fill = NA, lineheight = 0.4)
  }

  if (show_anatomical_labels) {
    anatomical_df <- dplyr::tibble(x=c(0,0,-3.6,3.6), y=c(3.5,-3.5,0,0), label=c("Anterior", "Inferior", "Septal", "Lateral"))
    p <- p + ggplot2::geom_text(data = anatomical_df, ggplot2::aes(x=x, y=y, label=label), inherit.aes = FALSE, family = font_family, color = font_color_labels, size = font_size_labels, fontface = "italic")
  }

  # --- 6. Add Faceting and Export ---
  if (!rlang::quo_is_null(facet_by_quo)) {
    p <- p + ggplot2::facet_wrap(dplyr::vars(!!facet_by_quo)) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 12, face = "bold"))
  }

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, width = width, height = height, units = units, dpi = dpi)
    invisible(p)
  } else {
    return(p)
  }
}

# Helper for a common pattern
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


#' Create a Bull's Eye Plot from a Numeric Vector
#'
#' This is a convenience wrapper around `plot_bullseye_from_df` for cases
#' where data is already summarized in a simple vector.
#'
#' @param data A numeric vector of 16 or 17 values.
#' @param ... All other arguments from `plot_bullseye_from_df` (e.g., `palette`,
#'   `model`, `filename`, `value_name`) can be used here.
#'
#' @param scale_limits A numeric vector of length 2, e.g. `c(0, 1)`, to set
#'   fixed limits for the color scale. Only applies to continuous scales.
#' @return A ggplot object.
#' @export
#' @examples
#' # --- Example 1: Basic plot from a vector ---
#' strain_values <- rnorm(17, -19, 4)
#' plot_bullseye_from_vector(data = strain_values)
#'
#' # --- Example 2: Customizing the plot ---
#' plot_bullseye_from_vector(
#'   data = strain_values,
#'   palette = "colorblind",
#'   value_name = "Strain (%)",
#'   model = 17,
#'   line_color = "black"
#' )
plot_bullseye_from_vector <- function(data, model = 17, value_name = "Value", palette = "default",
                                      text_display = "value",
                                      show_anatomical_labels = TRUE,
                                      line_color = "white", font_family = "sans",
                                      font_color_text = "black", font_size_text = 9,
                                      font_color_labels = "black", font_size_labels = 4,
                                      scale_limits = NULL,
                                      filename = NULL, width = 7, height = 7, units = "in", dpi = 300) {

  # --- Data Preparation ---
  coords <- get_segment_coords(model = model)
  use_discrete_scale <- TRUE
  breaks <- c(-Inf, -15, -10, -5, 0, 5, 10, 15, Inf)
  labels <- c("<= -15", "-14.9 to -10", "-9.9 to -5", "-4.9 to -0.0", "0.0 to 4.9", "5.0 to 9.9", "10.0 to 14.9", ">= 15")
  if (length(palette) == 1 && palette == "echo") {
    colors <- c("#f8120e", "#fe4546", "#fd7678", "#feb2b8", "#b4b4fc", "#7977ff", "#3a3cff", "#0101fe")
  } else if (length(palette) == 1 && palette == "default") {
    colors <- c("#006400", "#32CD32", "#9ACD32", "#FFFF00", "#FFD700", "#FFA500", "#FF4500", "#FF0000")
  } else if (length(palette) == 1 && palette == "colorblind") {
    colors <- viridisLite::viridis(8)
  } else {
    use_discrete_scale <- FALSE; colors <- palette
  }
  if (use_discrete_scale) { colors <- stats::setNames(rev(colors), labels) }
  plot_df <- dplyr::tibble(
    segment = 1:model, value = data
  )
  if (use_discrete_scale) {
    plot_df$value_binned <- factor(cut(plot_df$value, breaks, names(colors), right=F, include.lowest=T), levels = names(colors))
  }
  plot_data <- dplyr::left_join(coords, plot_df, by = "segment")

  # --- Plotting ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, group = segment)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = if(use_discrete_scale) value_binned else value), color = line_color, linewidth=1.2, show.legend=T) +
    ggplot2::coord_equal(xlim=c(-4, 4), ylim=c(-4, 4)) + ggplot2::theme_void() +
    ggplot2::theme(
      legend.position="right",
      legend.title = ggplot2::element_text(face = "bold"),
      text=ggplot2::element_text(family=font_family)
    )
  if (use_discrete_scale) {
    p <- p + ggplot2::scale_fill_manual(name=value_name, values=colors, drop=F, limits=names(colors))
  } else {
    # Use scale_limits if provided, otherwise use the data range
    final_limits <- scale_limits %||% range(plot_data$value, na.rm = TRUE)
    p <- p + ggplot2::scale_fill_gradientn(name=value_name, colors=colors, limits = final_limits)
  }

  if (text_display != "none") {
    label_data <- plot_data |>
      dplyr::group_by(segment, segment_name, value) |>
      dplyr::summarize(x = mean(x), y = mean(y), .groups = "drop") |>
      dplyr::mutate(
        display_text = dplyr::case_when(
          text_display == "name" ~ paste0("<b style='color:", font_color_text, "; font-size:7pt;'>", gsub(" ", "<br>", segment_name), "</b>"),
          TRUE ~ paste0("<b style='color:", font_color_text, "; font-size:", font_size_text, "pt;'>", round(value, 2), "</b>")
        ),
        x = ifelse(segment == 17, x, x * 1.05),
        y = ifelse(segment == 17, y, y * 1.05)
      )
    p <- p + ggtext::geom_richtext(
      data = label_data, ggplot2::aes(x = x, y = y, label = display_text),
      inherit.aes = FALSE, label.color = NA, fill = NA, lineheight = 0.4
    )
  }
  if (show_anatomical_labels) {
    anatomical_df <- dplyr::tibble(x=c(0,0,-3.55,3.55), y=c(3.4,-3.4,0,0), label=c("Anterior", "Inferior", "Septal", "Lateral"))
    p <- p + ggplot2::geom_text(data=anatomical_df, ggplot2::aes(x=x, y=y, label=label), inherit.aes=F, family=font_family, color=font_color_labels, size=font_size_labels, fontface="italic")
  }

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, width=width, height=height, units=units, dpi=dpi)
    invisible(p)
  } else {
    return(p)
  }
}
