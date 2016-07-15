base_envirologger_url <- function(user, key) {
  
  # Build base
  base_url <- "http://api.envirologger.net/2.0/interface/"
  base_url <- stringr::str_c(base_url, user, "/", key, "/")
  base_url
  
}


response_check <- function(x) {
  
  # Check
  if (grepl("authentication failed", x, ignore.case = TRUE))
    stop("API authentication failed.", call. = FALSE)
  
  if (grepl("no data", x, ignore.case = TRUE))
    stop("No data found by API.", call. = FALSE)
    
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
    date <- lubridate::parse_date_time(
      date, c("ymd", "ymd_h", "ymd_hm", "ymd_hms", "dmy", "dmy_h", "dmy_hm", "dmy_hms"), 
                                       tz = tz)
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
  x
}


arrange_left <- function (df, variable) {
  variable <- stringr::str_c("\\b", variable, "\\b")
  index <- lapply(variable, function(x) grep(x, names(df)))
  index <- index[lapply(index, length) > 0]
  index <- unlist(index)
  df <- df[, c(c(index), (1:ncol(df))[-index])]
  df
}


replicate_rows <- function (df, n, reset = TRUE) {
  if (reset) 
    row.names(df) <- NULL
  df <- df[rep(seq_len(nrow(df)), each = n), ]
  if (reset) 
    row.names(df) <- NULL
  df
}
