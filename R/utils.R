check_valid_code <- function(code) {
  if (!stringr::str_detect(code, "^[\\sa-zA-Z0-9_]+$")) {
    stop(paste0("`", code, "` is not a valid series code"), call. = TRUE)
  }
}

check_valid_date <- function(date) {
  stringr::str_detect(date, "\\d{4}-\\d{2}-\\d{2}") && nchar(date)==10
}


check_valid_vintage <- function(vintage) {
  (vintage == "current" || vintage == "latest" || vintage == "previous" ||
     check_valid_date(vintage)) ||
    stop(paste0("Invalid vintage `", vintage, "`. Options are a date (in form YYYY-MM-DD) or one of `current` (alias `latest`) or `previous`"), call. = TRUE)
}

