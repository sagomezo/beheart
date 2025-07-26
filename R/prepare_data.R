# In R/prepare_data.R

#' Reshape a Wide Data Frame to a Long Format for Plotting
#'
#' This helper function transforms a data frame from a wide format (one row per
#' subject, with segments in separate columns) to the long format required by
#' `plot_bullseye_from_df()`.
#'
#' @param data The wide-format data frame.
#' @param segment_map A named character or numeric vector. The names should be
#'   the column names in `data` that correspond to the segments, and the values
#'   should be the corresponding segment numbers (1-17).
#' @param id_cols A character vector of column names to keep as identifiers
#'   (e.g., patient ID, group, etc.). These columns will be preserved in the
#'   long-format data frame.
#' @param values_to The name of the new column that will store the segment values
#'   (e.g., strain measurements). Defaults to "value".
#'
#' @return A data frame in long format, ready for use with `plot_bullseye_from_df()`.
#' @export
#' @examples
#' # Create a sample wide-format data frame
#' wide_df <- data.frame(
#'   patient_id = c("Pat_01", "Pat_02", "Pat_03"),
#'   group = c("A", "B", "A"),
#'   basal_ant = c(-20.1, -22.4, -19.8),
#'   basal_sept = c(-21.5, -23.1, -20.5),
#'   mid_ant = c(-18.9, -20.0, -19.2)
#' )
#'
#' # Create the map linking column names to segment numbers
#' seg_map <- c(
#'   "basal_ant" = 1,
#'   "basal_sept" = 2,
#'   "mid_ant" = 7
#' )
#'
#' # Reshape the data
#' long_df <- prepare_bullseye_data(
#'   data = wide_df,
#'   segment_map = seg_map,
#'   id_cols = c("patient_id", "group"),
#'   values_to = "strain"
#' )
#'
#' print(long_df)
prepare_bullseye_data <- function(data, segment_map, id_cols = NULL, values_to = "value") {
  # Get the column names that correspond to segments
  segment_cols <- names(segment_map)

  # Check if all specified segment columns exist in the data
  missing_cols <- setdiff(segment_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following columns from `segment_map` are not in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Pivot the data to long format
  long_df <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(segment_cols),
    names_to = ".original_col",
    values_to = values_to
  )

  # Map the original column names to the standard segment numbers
  long_df$segment <- as.integer(segment_map[long_df$.original_col])

  # Remove the temporary original column name
  long_df$.original_col <- NULL

  # Reorder columns for clarity
  dplyr::relocate(long_df, dplyr::any_of(id_cols), segment, !!rlang::sym(values_to))
}
