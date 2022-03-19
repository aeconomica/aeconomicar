#' Fetch dataset with dataset_id `dataset_id`.
#'
#' @param dataset_id The dataset id you want to fetch. You can find dataset codes
#' on the Aeconomica website in the top right of the page showing the dataset.
#' @param restrictions (optional) A list with keys (corresponding to names of
#' dimensions) mapping to a vector of codes. Only series that have those codes
#' for that dimension are returned in the dataset.
#' @param vintage (default "latest") Specify a vintage for the series in the
#' dataset to fetch. Cannot be used if your specify vintages for individual series.
#' Vintage can be any of `current` (alias `latest`, default), `previous` or a
#' date in YYYY-MM-DD form.
#' @param dimensions can be one of "code" or "name." This dictates whether the
#' values for each dimension of the dataset are returned as the codes for each
#' dimension, or the long names that correspond to those codes.
#'
#' @examples
#' \dontrun{
#' fetch_dataset("WJP_STATES")
#' }
#'
#' @export
fetch_dataset <- function(dataset_id, restrictions = list(), vintage = "latest", dimensions = "code") {
  check_valid_vintage(vintage)
  check_valid_code(dataset_id)

  if (dimensions != "code" && dimensions != "name") {
    stop("`dimensions` can only be one of `code` or `name`", call. = FALSE)
  }

  if (length(restrictions) > 0) {
    restrictions_string <- paste0("\"restrictions\" : ", jsonlite::toJSON(restrictions), ",")
  } else {
    restrictions_string <- ""
  }

  res <- httr::POST(
    "https://aeconomica.io//api/v1/dataset",
    encode = "json",
    body = paste0(
      "{
      \"dataset\": \"", dataset_id, "\",
        ", restrictions_string, "
      \"vintage\" : \"", vintage,"\",
      \"apikey\" : \"", apikey(), "\" }"
    )
  )

  if (httr::status_code(res) == 200) {
    res <- httr::content(res)
    res <- purrr::map_dfc(res, function(col) unlist(purrr::map(col, ~ ifelse(is.null(.x), NA, .x))))
  } else {
    errmsg <- httr::content(res)[["error"]]
    if (substr(errmsg, 1,21) == "500 Internal Error - ") {
      errmsg <- substr(errmsg, 22, nchar(errmsg))
      stop(errmsg, call. = TRUE)
    } else {
      stop(errmsg, call. = TRUE)
    }
  }

  if (dimensions == "code") {
    return(res)
  } else {
    structure <- httr::POST(
      "https://aeconomica.io//api/v1/dataset_structure",
      encode = "json",
      body = paste0(
        "{
          \"dataset\": \"", dataset_id, "\",
          \"apikey\" : \"", apikey(), "\" }"
      )
    )
    if (httr::status_code(structure) == 200) {
      structure <- httr::content(structure)
      for (dim in structure$dimensions) {
        res[ , dim$dimname] <- unlist(purrr::map(
          res[ , dim$dimname][[1]],
          function(code) {
            return(dim$options[[match(code, unlist(purrr::map(dim$options, "code")))]]$name)
          }
        ))
      }
      return(res)
    } else {
      errmsg <- httr::content(structure)[["error"]]
      if (substr(errmsg, 1,21) == "500 Internal Error - ") {
        errmsg <- substr(errmsg, 22, nchar(errmsg))
        stop(errmsg, call. = TRUE)
      } else {
        stop(errmsg, call. = TRUE)
      }
    }

  }
}
