#' Function to get observational data from Envirologger API. 
#' 
#' The Envirologger API is not fast and will take about a minute to download a
#' day's worth of one-minute data. By default, this function only queries three-
#' hours worth of data at a time to keep queries manageable for the API. If the
#' amount of data requested is too high, the interface is unreliable as the API
#' begins to raise error codes and can hang. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user An Envirologger API user-name. 
#' 
#' @param key An Envirologger API key for \code{user}. 
#' 
#' @param server An Envirologger API server code to use for download. Server 
#' codes are integers and \code{\link{get_envirologger_stations}} can be used to 
#' find these codes. 
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
#' @param tz Time-zone for the observations' dates. The default is \code{"UTC"}.
#' 
#' @param remove_duplicates Should "true" date-station-variable-value duplicates
#' be removed? Default is \code{TRUE} as this is common. 
#' 
#' @param interval How much data should the function request from the API for 
#' each iteration? Default is \code{"3 hour"}. 
#' 
#' @param progress Type of progress bar to display. Default is \code{"time"}. 
#' 
#' @param print_query Should the API query strings be printed? Default is 
#' \code{FALSE}. 
#' 
#' @return Data frame with correct data types. 
#' 
#' @seealso \href{http://api.envirologger.net/2.0/documentation}{API Documentation},
#' \code{\link{get_envirologger_stations}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get some data for a made up station
#' data_air <- get_envirologger_data(user, key, 100, 1000, start = "2016-06-20", 
#'                                   end = "2016-06-21")
#' 
#' }
#' 
#' @import dplyr
#' @import stringr
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_envirologger_data <- function(user, key, server, station, start = NA, 
                                  end = NA, tz = "UTC", remove_duplicates = TRUE, 
                                  interval = "3 hour", progress = "time",
                                  print_query = FALSE) {
  
  # Build query strings for api
  urls <- build_query_urls(user, key, server, station, start, end, interval)
  
  # Get data
  df <- plyr::ldply(urls, get_data_worker, tz = tz, print_query = print_query, 
                    .progress = progress)
  
  if (!nrow(df) == 0) {
    
    # Clean names
    names(df) <- str_underscore(names(df))
    names(df) <- ifelse(names(df) == "pre_scaled", "value", names(df))
    
    # Lower case and trim
    df$label <- str_to_lower(df$label)
    df$label <- str_trim(df$label)
    
    # Remove true value duplicates
    if (remove_duplicates) {
      
      df <- df %>% 
        distinct(date,
                 station,
                 channel_number,
                 value,
                 .keep_all = TRUE)
      
    }
    
    # Arrange
    df <- arrange_left(df, c("date", "station", "label", "sensor", "value"))
    
  } else {
    
    # No data to return
    df <- data.frame()
    
  }
  
  # Return
  df
  
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
  date_system <- lubridate::floor_date(Sys.time(), "day")
  date_system <- lubridate::force_tz(date_system, "UTC")
  
  # Push
  if (date_system == end) end <- end + lubridate::days(1)
  
  # Create mapping data frame, quite a bit of work and there still is overlap
  df <- data.frame(date = seq(start, end, interval)) %>% 
    mutate(date_end = lead(date),
           date_end_lag = lag(date_end),
           date_end_lag = ifelse(is.na(date_end_lag), date_end, date_end_lag),
           date = ifelse(is.na(date), date_end, date),
           date = ifelse(date == date_end_lag, date + 60, date),
           date = as.POSIXct(date, tz = "UTC", origin = "1970-01-01"),
           date = str_replace_all(date, "-|:| ", ""), 
           date_end = str_replace_all(date_end, "-|:| ", ""),
           date = str_sub(date, end = 10), 
           date_end = str_sub(date_end, end = 10)) %>% 
    filter(!is.na(date_end)) %>% 
    select(-date_end_lag)
  # Could use end = 12, but issues arrise, first query is ignored. API behaviour? 
  
  # Vectorise over site too
  if (length(station) == 1) {
    
    # Just add station
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
  url <- str_c("stationdata/bydate/", server, "/", df$station, "/", 
               df$date, "/", df$date_end)
  
  # Add base of url 
  url <- str_c(base_envirologger_url(user, key), url)
  
  # Return
  url
  
}


# Function to get read json return and format into a data frame 
#
# No export
get_data_worker <- function(url, tz, print_query) {
  
  # Message query string
  if (print_query) message(url)
  
  # Get station from url
  station <- str_split_fixed(url, "/", 13)[, 11]
  station <- as.integer(station)
  
  # Get response
  response <- tryCatch({
    
    # Get response as text
    text <- readLines(url, warn = FALSE)
    
    # Check response
    response_check(text)
    
    # Return
    text
    
  }, warning = function(w) {
    
    # Return
    NULL
    
  }, error = function(e) {
    
    # Return
    NULL
    
  })
  
  # If we get a return, make a nice data frame
  if (!is.null(response)) {
    
    # Catch is for rare issues with EOF errors caused by api
    response <- tryCatch({
      
      # Parse
      fromJSON(response)
      
    }, error = function(e) {
      
      # Return
      NULL
      
    })
    
    # Another catch
    if (!is.null(response)) {
      
      # Get dates
      date <- response$Timestamp
      
      # Parse dates
      date <- lubridate::ymd_hms(date, tz = tz)
      
      # Get observations
      df <- response$Channels
      
      # Insert date into observations, an odd piece of code
      df <- mapply(cbind, df, "date" = date, SIMPLIFY = FALSE)
      
      # Create data frame
      df <- bind_rows(df)
      
      # Add station key
      df$station <- station
      
    }
    
  } else {
    
    # Return empty data frame, reassign tryCatch return
    df <- data.frame()
    
  }
  
  # Return
  df
  
}
