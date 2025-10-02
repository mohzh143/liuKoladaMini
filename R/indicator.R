#' List or search indicators (KPI) from the Kolada API
#'
#' This function retrieves a list of key performance indicators (KPIs)
#' from the Kolada database. You can optionally search for indicators
#' by providing a title substring.
#'
#' @param q A character string to search for within the indicator titles.
#'   Defaults to `NULL`, which returns all indicators.
#' @return A `tibble` with columns:
#'   \describe{
#'     \item{id}{The unique identifier for the indicator.}
#'     \item{title}{The name of the indicator.}
#'     \item{category}{The category the indicator belongs to, if available.}
#'   }
#' @export
#' @examples
#' # Get all available indicators
#' all_indicators <- kld_indicators()
#' head(all_indicators)
#'
#' # Search for indicators related to "arbetslöshet" (unemployment, KPI N02282)
#' unemployment_indicators <- kld_indicators("arbetslöshet")
#' head(unemployment_indicators)
kld_indicators <- function(q = NULL) {
  req <- httr2::request("https://api.kolada.se/v2/kpi")
  if (!is.null(q)) req <- httr2::req_url_query(req, title = q)
  resp <- httr2::req_url_query(req, page = 1, page_size = 5000) |>
    httr2::req_perform()

  js <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
  vals <- js$values
  if (is.null(vals)) {
    cand <- js[vapply(js, is.data.frame, logical(1))]
    if (length(cand)) vals <- cand[[1]]
  }
  df <- tibble::as_tibble(vals)
  dplyr::select(df, dplyr::any_of(c("id","title","category")))
}
