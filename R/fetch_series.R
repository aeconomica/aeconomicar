#' Fetch a series, or list of series, of economic data from the Aeconomica API.
#'
#' @param series A series, or number of series to fetch. Can be either a single
#' series code, a vector of series codes, or a list of series codes and vintage
#' pairs (with the series code as the name and the vintage as the value). You
#' can find series codes on the Aeconomica website in the top right of the page
#' showing the series.
#' @param vintage (optional) Specify a vintage for all of the series you want
#' to fetch. Cannot be used if your specify vintages for individual series.
#' Vintage can be any of `current` (alias `latest`, default), `previous` or a
#' date in YYYY-MM-DD form.
#'
#' @examples
#' \dontrun{
#' # Downloading a single series
#' fetch_series("WPI")
#'
#' # Downloading a number of series
#' fetch_series(c("GDP_YE", "CPI_YE"))
#'
#' # Use a list to download a different series with different vintages
#' fetch_series(list("GDP_YE" = "latest", "CPI_YE" = "previous"))
#' }
#'
#' @export
fetch_series <- function(series, vintage = "latest") {
  if (!is.list(series) && length(series) == 1) {
    series <- list(c(series, vintage))
  } else if (!is.list(series) && is.vector(series)) {
    series <- purrr::map(series, function(s) c(s, vintage))
  } else {
    series <- purrr::map2(
      names(series), series,
      function(s, v) {
        if (s == "") {
          stop("If providing a list, each element must the series codes as the name and the vintage as the value, e.g. list(code = \"latest\", code2 = \"previous\")", call. = TRUE)
        } else {
          return(c(s,v))
        }
      }
    )
  }

  purrr::map(series, function(s) {
    check_valid_code(s[1])
    check_valid_vintage(s[2])
  })

  series_req <- paste(
    purrr::map(series, function(p) paste0("{ \"id\" : \"", p[1], "\", \"vintage\" : \"", p[2], "\" }")),
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
      df <- tibble::tibble(
        series_id = x$series_id,
        vintage = x$vintage,
        dates = unlist(x$dates),
        values = unlist(purrr::map(x$values, ~ ifelse(is.null(.x), NA_real_, .x)))
      )
    })
    return(purrr::reduce(res, dplyr::bind_rows))
  } else {
    errmsg <- httr::content(res)[["error"]]
    if (is.character(errmsg) && substr(errmsg, 1, 21) == "500 Internal Error - ") {
      errmsg <- substr(errmsg, 22, nchar(errmsg))
      stop(errmsg, call. = TRUE)
    } else {
      if (is.list(errmsg) && ("message" %in% names(errmsg))) {
        stop(errmsg[["message"]], call. = TRUE)
      } else {
        stop(errmsg, call. = TRUE)
      }
    }
  }
}
