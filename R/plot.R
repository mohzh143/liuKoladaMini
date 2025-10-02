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
#' # First, retrieve some data using kld_values()
#' # This example uses a mock directory to ensure reproducibility.
#' # In a real scenario, this would call the live API.
#' \dontrun{
#'   # Assumes a mock fixture exists for this API call.
#'   df <- kld_values("0580", "N02282", 2019:2024)
#'
#'   # Create the plot
#'   kld_plot(df)
#' }
kld_plot <- function(df) {
  if (!all(c("year","value") %in% names(df))) {
    stop("Input data must contain columns: 'year' and 'value'.", call. = FALSE)
  }
  ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Year", y = "Value")
}
