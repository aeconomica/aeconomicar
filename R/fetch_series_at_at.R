#' Fetch a series, or list of series, of economic data from the Aeconomica API
#' as that series was at some point in time.
#'
#' @param series A series, or number of series to fetch. Can be either a single
#' series code, a vector of series codes, or a list of series codes and vintage
#' pairs (with the series code as the name and the vintage as the value). You
#' can find series codes on the Aeconomica website in the top right of the page
#' showing the series.
#' @param as_at_date Fetch the series vintage that was the latest vintage of
#  the series that was available as at the date specified. Date must be in
#' YYYY-MM-DD form.
#'
#' @examples
#' \dontrun{
#' # Downloading a single series as it was 23 September 2021
#' fetch_series_as_at("WPI", "2021-09-23")
#'
#' # Downloading a number of series
#' fetch_series_as_at(c("GDP_YE", "CPI_YE"), "21-09-23")
#'
#' # Use a list to download a different series with different `as_at_date`s
#' fetch_series_as_at(list("GDP_YE" = "21-09-23", "CPI_YE" = "2020-12-23"))
#' }
#'
#' @export
fetch_series_as_at <- function(series, as_at_date) {
  if (!is.list(series) && length(series) == 1) {
    series <- list(c(series, as_at_date))
  } else if (!is.list(series) && is.vector(series)) {
    series <- purrr::map(series, function(s) c(s, as_at_date))
  } else {
    series <- purrr::map2(
      names(series), series,
      function(s, v) {
        if (s == "") {
          stop("If providing a list, each element must the series codes as the name and the `as_at_date` as the value, e.g. list(code = \"YYYY-MM-DD\", code2 = \"YYYY-MM-DD\")", call. = TRUE)
        } else {
          return(c(s,v))
        }
      }
    )
  }

  purrr::map(series, function(s) {
    check_valid_code(s[1])
    if (!check_valid_date(s[2])) {
      stop(
        paste0("Invalid as_at_date `", s[2], "`. Date must be in form `YYYY-MM-DD`."),
        call. = FALSE
      )
    }
  })

  series_req <- paste(
    purrr::map(series, function(p) paste0("{ \"id\" : \"", p[1], "\", \"as_at_date\" : \"", p[2], "\" }")),
    collapse = ", "
  )

  res <- httr::POST(
    "https://aeconomica.io/api/v1/fetchseries",
    encode = "json",
    body = paste0(
      "{
      \"series\": [
        ", series_req, "
      ],
      \"apikey\" : \"", apikey(), "\" }"
    )
  )

  if (httr::status_code(res) == 200) {
    res <- httr::content(res)
    res <- purrr::map(res, function(x) {
      tibble::tibble(
        series_id = x$series_id,
        vintage = x$vintage,
        dates = unlist(x$dates),
        values = unlist(purrr::map(x$values, ~ ifelse(is.null(.x), NA_real_, .x)))
      )
    })
    return(purrr::reduce(res, dplyr::bind_rows))
  } else {
    errmsg <- httr::content(res)[["error"]]
    if (httr::status_code(res) == 400) {
      errmsg <- substr(errmsg, 19, nchar(errmsg))
      stop(errmsg, call. = TRUE)
    } else if (httr::status_code(res) == 401) {
      stop("Authorization required. Did you forget to provide an API key?")
    } else if (httr::status_code(res) == 403) {
      stop("Unauthorized. Check your API key and try again, or you may not have permissions for the requested resource.")
    } else {
      if (is.list(errmsg) && ("message" %in% names(errmsg))) {
        stop(errmsg[["message"]], call. = TRUE)
      } else {
        stop(errmsg, call. = TRUE)
      }
    }
  }
}
