base_envirologger_url <- function(user, key) {
  
  # Build base
  base_url <- "https://api.airmonitors.net/3.0/interface/"
  base_url <- stringr::str_c(base_url, user, "/", key, "/")
  return(base_url)
  
}


response_check <- function(x) {
  
  # Check
  if (any(grepl("authentication failed", x, ignore.case = TRUE)))
    stop("API authentication failed...", call. = FALSE)
  
  if (any(grepl("no data", x, ignore.case = TRUE)))
    stop("No data found by API...", call. = FALSE)
    
}


parse_date_arguments <- function (date, type, tz = "UTC") {
  if (is.na(date)) {
    date <- lubridate::ymd(Sys.Date(), tz = tz)
  }
  else {
    date_system <- lubridate::ymd(Sys.Date(), tz = tz)
    if (type == "start") {
      if (stringr::str_count(date) == 4) 
        date <- stringr::str_c(date, "-01-01")
      date <- ifelse(is.na(date), as.character(lubridate::floor_date(date_system, 
                                                                     "year")), date)
    }
    if (type == "end") {
      if (stringr::str_count(date) == 4) 
        date <- stringr::str_c(date, "-12-31")
      date <- ifelse(is.na(date), as.character(lubridate::ceiling_date(date_system, 
                                                                       "year")), date)
    }
    # Parse date
    date <- lubridate::parse_date_time(date, c("ymd", "dmy"), tz = tz)
  }
  date
}


str_underscore <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  return(x)
}


replicate_rows <- function (df, n, reset = TRUE) {
  if (reset) 
    row.names(df) <- NULL
  df <- df[rep(seq_len(nrow(df)), each = n), ]
  if (reset) 
    row.names(df) <- NULL
  return(df)
}


str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}

date_message <- function() stringr::str_c(str_date_formatted(), ": ")
