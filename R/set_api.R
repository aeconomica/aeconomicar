apikey <- function() {
  if (is.null(getOption("aeconomica.apikey")) || getOption("aeconomica.apikey") == "") {
    stop("You have not set an `apikey`. Please run `set_apikey(\"YOURKEY\")` first.", call. = TRUE)
  } else {
    getOption("aeconomica.apikey")
  }
}

#' Set API key to access Aeconomica API
#'
#' Visit your Aeconomica account page to find your API key.
#' 
#' Alternatively, you can set the environmental variable `AECONOMICA_APIKEY`, which will
#' be loaded at package load time.
#' 
#' You can overwrite this key at anytime by calling `set_apikey` again.
#'
#' @param apikey The API key for your Aeconomica account.
#'
#' @export
set_apikey <- function(apikey) {
  options(aeconomica.apikey = apikey)
}
