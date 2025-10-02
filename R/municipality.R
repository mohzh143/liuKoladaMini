#' List municipalities from the Kolada API
#'
#' This function retrieves a list of all municipalities and counties available
#' in the Kolada database.
#'
#' @return A `tibble` with columns:
#'   \describe{
#'     \item{id}{The unique identifier for the municipality or county (e.g., "0180" for Linköping).}
#'     \item{title}{The name of the municipality or county (e.g., "Linköping").}
#'     \item{type}{The type of the geographical entity ("county" or "municipality").}
#'   }
#' @export
#' @examples
#' # Get a list of all municipalities and counties
#' all_municipalities <- kld_municipalities()
#' head(all_municipalities)
#' tail(all_municipalities)
kld_municipalities <- function() {
  resp <- httr2::request("https://api.kolada.se/v2/municipality") |>
    httr2::req_url_query(page = 1, page_size = 5000) |>
    httr2::req_perform()

  js <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)

  df <- tibble::as_tibble(js$values)
  dplyr::select(df, dplyr::any_of(c("id","title","type")))
}
