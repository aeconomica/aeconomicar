.onLoad <- function(libname, pkgname){
  x <- Sys.getenv("AECONOMICA_APIKEY")
  if (x != "") {
    set_apikey(x)
  }
}