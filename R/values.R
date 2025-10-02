#' Get KPI values for a municipality from the Kolada API
#'
#' Retrieve time-series values for a specific key performance indicator (KPI)
#' for a given Swedish municipality.
#'
#' @param municipality_id Character scalar: municipality ID (e.g. `"0580"` for
#'   Linköping). Use [kld_municipalities()] to look up IDs.
#' @param kpi_id Character scalar: KPI/indicator ID (e.g. `"N02282"` for
#'   unemployment). Use [kld_indicators()] to look up IDs.
#' @param period Optional integer vector of years to filter (e.g. `2019:2024`).
#'   If `NULL`, all available years are returned.
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Year of the observation (integer).}
#'     \item{value}{KPI value for that year (numeric).}
#'     \item{municipality_id}{Municipality ID used in the query.}
#'     \item{kpi_id}{KPI/indicator ID used in the query.}
#'   }
#' @export
#' @examples
#' # Example: Unemployment among foreign-born (N02282) in Linköping (0580),
#' # for years 2019–2024
#' linkoping_unemp <- kld_values(
#'   municipality_id = "0580",
#'   kpi_id = "N02282",
#'   period = 2019:2024
#' )
#' linkoping_unemp
kld_values <- function(municipality_id, kpi_id, period = NULL) {
  stopifnot(is.character(municipality_id), length(municipality_id) == 1L,
            is.character(kpi_id),          length(kpi_id)          == 1L)

  # Path-style
  req <- httr2::request(
    sprintf("https://api.kolada.se/v2/data/kpi/%s/municipality/%s", kpi_id, municipality_id)
  )
  if (!is.null(period)) {
    req <- httr2::req_url_query(req, year = paste(period, collapse = ","))
  }

  resp <- httr2::req_perform(req)
  js   <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
  df   <- tibble::as_tibble(js$values)

  # Normalize year column
  if ("year" %in% names(df)) {
    df$year <- suppressWarnings(as.integer(df$year))
  } else if ("period" %in% names(df)) {
    df$year <- suppressWarnings(as.integer(df$period))
  }

  # Normalize value column
  if ("value" %in% names(df)) {
    df$value <- suppressWarnings(as.numeric(df$value))
  } else if ("values.value" %in% names(df)) {
    df$value <- suppressWarnings(as.numeric(df[["values.value"]]))
  } else if ("values" %in% names(df)) {
    extract_scalar_num <- function(x) {
      if (is.atomic(x)) return(suppressWarnings(as.numeric(x[1])))
      if (is.data.frame(x)) {
        if ("value" %in% names(x)) return(suppressWarnings(as.numeric(x$value[1])))
        return(suppressWarnings(as.numeric(x[[1]][1])))
      }
      if (is.list(x)) {
        if (!is.null(x$value)) return(extract_scalar_num(x$value))
        if (length(x) >= 1)    return(extract_scalar_num(x[[1]]))
      }
      NA_real_
    }
    df$value <- vapply(df$values, extract_scalar_num, numeric(1))
  }

  # Add missing ID columns
  if (!"municipality_id" %in% names(df)) df$municipality_id <- municipality_id
  if (!"kpi_id"         %in% names(df)) df$kpi_id          <- kpi_id

  # Apply period filter
  if (!is.null(period) && "year" %in% names(df)) {
    yr <- as.integer(period)
    df <- df[df$year %in% yr, , drop = FALSE]
  }

  # Select and order columns
  keep <- intersect(c("year","value","municipality_id","kpi_id"), names(df))
  df <- df[, keep, drop = FALSE]
  if ("year" %in% names(df)) df <- df[order(df$year), , drop = FALSE]
  return(df)
}
