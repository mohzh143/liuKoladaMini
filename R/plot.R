#' Create a quick line plot from kld_values() output
#'
#' This function generates a simple line and point plot for time-series data
#' retrieved using the `kld_values()` function. It provides a convenient
#' way to visualize the data without manually setting up a `ggplot` object.
#'
#' @importFrom rlang .data
#'
#' @param df A `tibble` data frame returned by the `kld_values()` function.
#'   It is expected to contain at least the "year" and "value" columns.
#'
#' @return A `ggplot` object that can be further customized.
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- kld_values("0580", "N02282", 2019:2024)
#'   kld_plot(df)
#' }
kld_plot <- function(df) {
  if (!all(c("year", "value") %in% names(df))) {
    stop("Input data must contain columns: 'year' and 'value'.", call. = FALSE)
  }

  # Auto-generate title
  title_txt <- if ("municipality_id" %in% names(df) && "kpi_id" %in% names(df)) {
    paste0("KPI ", unique(df$kpi_id), " in municipality ", unique(df$municipality_id))
  } else {
    "KPI values"
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value, group = 1)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "darkred") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(x = "Year", y = "Value", title = title_txt) +
    ggplot2::theme_minimal()
}
