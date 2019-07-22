#' Function to get observational data from Envirologger API. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user An Envirologger API user-name. 
#' 
#' @param key An Envirologger API key for \code{user}. 
#' 
#' @param station A vector of station codes to download. Station codes are 
#' integers and \code{\link{get_envirologger_stations}} can be used to find 
#' these codes. 
#' 
#' @param start What is the start date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and
#' will floor-rounded. 
#' 
#' @param end What is the end date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and 
#' will be ceiling-rounded. 
#' 
#' @param tz Time zone for the observations' dates. The default is \code{"UTC"}.
#' 
#' @param remove_duplicates Should "true" date-station-variable-value duplicates
#' be removed? Default is \code{TRUE} as this is common. 
#' 
#' @param interval How much data should the function request from the API for 
#' each iteration? 
#' 
#' @param verbose Should the functions give messages? 
#' 
#' @return Tibble. 
#' 
#' @seealso \href{https://api.airmonitors.net/3.5/documentation}{API Documentation},
#' \code{\link{get_envirologger_stations}}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Get some data for a made up station
#' data_air <- get_envirologger_data(
#'   user = user, 
#'   key = key, 
#'   station = 1001, 
#'   start = "2016-06-20", 
#'   end = "2016-06-21"
#' )
#' 
#' }
#' 
#' @export
get_envirologger_data <- function(user, key, station, start = NA, 
                                  end = NA, tz = "UTC", remove_duplicates = TRUE, 
                                  interval = "24 hour", verbose = FALSE) {
  
  # Build query strings for api
  urls <- build_query_urls(user, key, server, station, start, end, interval)
  
  # Get data
  df <- purrr::map_dfr(
    urls, 
    get_envirologger_data_worker, 
    tz = tz, 
    verbose = verbose
  )
  
  if (!nrow(df) == 0) {
    
    # Clean names
    names(df) <- str_to_underscore(names(df))
    names(df) <- if_else(names(df) == "pre_scaled", "value", names(df))
    
    # Remove true value duplicates
    if (remove_duplicates) {
      df <- df %>% 
        distinct(date,
                 station,
                 channel,
                 value,
                 .keep_all = TRUE)
    }
    
    # Arrange variable order and arrange observations
    df <- df %>% 
      select(date, 
             date_end,
             station, 
             sensor_label, 
             value, 
             everything()) %>% 
      arrange(station, 
              channel,
              date)
    
  } else {
    df <- tibble()
  }
  
  return(df)
  
}


# Function to build urls to send to API
#
# No export
build_query_urls <- function(user, key, server, station, start, end, interval) {
  
  # Parse arguments
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Push end date if needed
  # For when both na
  if (start == end) end <- end + lubridate::days(1)
  
  # For when date end is today
  date_system <- lubridate::floor_date(lubridate::now(), "day")
  date_system <- lubridate::force_tz(date_system, tz = "UTC")
  
  # Push
  if (date_system == end) end <- end + lubridate::days(1)
  
  # Create mapping data frame, quite a bit of work and there still is overlap
  df <- tibble(date = seq(start, end, interval)) %>% 
    mutate(date_end = dplyr::lead(date),
           date_end_lag = dplyr::lag(date_end),
           date_end_lag = if_else(is.na(date_end_lag), date_end, date_end_lag),
           date = if_else(is.na(date), date_end, date),
           date = if_else(date == date_end_lag, date + 60, date),
           date = parsedate::format_iso_8601(date),
           date_end = parsedate::format_iso_8601(date_end)) %>% 
    filter(!is.na(date_end)) %>% 
    select(-date_end_lag)
    
  # Could use end = 12, but issues arrise, first query is ignored. API behaviour? 
  
  # Vectorise over site too
  if (length(station) == 1) {
    df$station <- station
  } else {
    
    # Replicate dates
    df <- replicate_rows(df, length(station))
    
    # Add station vector
    df$station <- station
    
    # Arrange by station
    df <- arrange(df, station)
    
  }
  
  # Build query strings
  url <- stringr::str_c(
    "stationdata/date/", 
    df$date, "/", 
    df$date_end, "/",
    df$station
  )
  
  # Add base of url 
  url <- stringr::str_c(base_envirologger_url(user, key), url)
  
  return(url)
  
}


# Function to get read json return and format into a data frame 
#
# No export
get_envirologger_data_worker <- function(url, tz, user, key, verbose) {
  
  # Message date and station codes
  if (verbose) {
    stringr::str_split_fixed(url, "date/", 2)[, 2] %>% 
      message(date_message(), ., "...")
  }
  
  # Get station from url
  station <- stringr::str_split_fixed(url, "/", 12)[, 12]
  station <- as.numeric(station)
  
  # Get response
  response <- tryCatch({
    
    # Get response as text
    text <- readLines(url, warn = FALSE)
    response_check(text)
    text
    
  }, warning = function(w) {
    NULL
  }, error = function(e) {
    NULL
  })
  
  # Check for discontinued string, null behaves differently in the logic
  if (!is.null(response)) {
    if (any(grepl("discontinued", response, ignore.case = TRUE))) {
      # For the user
      warning("API is reporting that it has been discontinued...", call. = FALSE)
      # Keep logic the same
      response <- NULL
    }
  }
  
  # If we get a return, make a nice data frame
  if (!is.null(response)) {
    
    # Catch is for rare issues with EOF errors caused by api
    response <- tryCatch({
      jsonlite::fromJSON(response)
    }, error = function(e) {
      NULL
    })
    
    # Another catch
    if (!is.null(response)) {
      
      # Get dates
      date <- lubridate::ymd_hms(response$TBTimestamp, tz = tz)
      date_end <- lubridate::ymd_hms(response$TETimestamp, tz = tz)
      
      # Get observations
      df <- response$Channels
      
      # Insert date into observations, an odd piece of code
      df <- mapply(cbind, df, "date" = date, SIMPLIFY = FALSE)
      df <- mapply(cbind, df, "date_end" = date, SIMPLIFY = FALSE)
      
      # Create data frame
      df <- dplyr::bind_rows(df)
      
      # Add station key
      df$station <- station
      
      # Represent missing ness with NAs
      df <- df %>% 
        mutate(PreScaled = if_else(PreScaled == -999, NA_real_, PreScaled),
               Scaled = if_else(Scaled == -999, NA_real_, Scaled)) %>% 
        as_tibble()
      
      # Status = stringr::str_to_lower(Status),
      # Status = stringr::str_replace_all(Status, " ", "_")
      
    }
    
  } else {
    df <- tibble()
  }
  
  return(df)
  
}
