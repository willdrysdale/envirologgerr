#' Function to get data from Envirologger API. 
#' 
#' The Envirologger API is not fast. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user Envirologger API user-name. 
#' 
#' @param key Envirologger API key for \code{user}. 
#' 
#' @param station Station's data to download. 
#' 
#' @param server API server to use for download. 
#' 
#' @param start Start date of query.
#' 
#' @param end End date of query. 
#' 
#' @param tz Time-zone for the observations' dates
#' .
#' @param extra Should the returned data frame contain extra infomation? Default
#' is \code{TRUE} but setting to \code{FALSE} can be useful for packages such
#' as \strong{openair}. 
#' 
#' @param interval How much data should the function request from the API for 
#' each iteration? Default is \code{"3 hour"}. 
#' 
#' @param progress Type of progress bar to display. Default is \code{"time"}. 
#' 
#' @seealso \href{http://api.envirologger.net/2.0/documentation}{Documentation},
#' \code{\link{get_envirologger_stations}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get some data for a made up station
#' data_air <- get_envirologger_data(user, key, 1000, 100, start = "2016-06-20", 
#'                                   end = "2016-06-21")
#' 
#' }
#' 
#' @import dplyr
#' @import stringr
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_envirologger_data <- function(user, key, station, server, start = NA, end = NA, 
                                  tz = "UTC", extra = TRUE, interval = "3 hour", 
                                  progress = "time") {
  
  # Build query strings for api
  urls <- build_query_urls(user, key, server, station, start, end, interval)
  
  # Get data
  df <- plyr::ldply(urls, get_data_worker, tz = tz, .progress = progress)
  
  if (!nrow(df) == 0) {
    
    # Clean names
    names(df) <- str_underscore(names(df))
    names(df) <- ifelse(names(df) == "pre_scaled", "value", names(df))
    
    # Lower case and trim
    df$label <- str_to_lower(df$label)
    df$label <- str_trim(df$label)
    
    # Arrange
    df <- arrange_left(df, c("date", "station", "label", "sensor", "value"))
    
    if (!extra) {
      
      df <- tryCatch({
        
        df %>% 
          select(date,
                 station,
                 label,
                 value) %>% 
          tidyr::spread(label, value)
        
      }, error = function(e) {
        
        # Warning to the user
        warning("Duplicate date-station-label observations detected, data have been removed.", 
                call. = FALSE)
        
        df %>% 
          select(date,
                 station,
                 label,
                 value) %>% 
          distinct(date,
                   station, 
                   label) %>% 
          tidyr::spread(label, value)
        
      })
      
    }
    
  } else {
    
    # No data to return
    df <- NULL
    
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
  
  # Push
  if (start == end) end <- end + lubridate::days(1)
  
  # Create mapping data frame sequence
  df <- data.frame(date = seq(start, end, interval)) %>% 
    mutate(date_end = lead(date),
           # date_end = date_end - 1,
           date = str_replace_all(date, "-|:| ", ""), 
           date_end = str_replace_all(date_end, "-|:| ", ""),
           date = str_sub(date, end = 10), 
           date_end = str_sub(date_end, end = 10)) %>% 
    filter(!is.na(date_end))
  
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
get_data_worker <- function(url, tz) {
  
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
    
    NULL
    
  }, error = function(e) {
    
    NULL
    
  })
  
  # If we get a return, make a nice data frame
  if (!is.null(response)) {
    
    # Parse text
    response <- fromJSON(response)
    
    # Get dates
    date <- response$Timestamp
    
    # Parse dates
    date <- lubridate::ymd_hms(date, tz = tz)
    
    # Get observations
    df <- response$Channels
    
    # Insert date into observations
    df <- mapply(cbind, df, "date" = date, SIMPLIFY = FALSE)
    
    # Create data frame
    df <- bind_rows(df)
    
    # Add station key
    df$station <- station
    
  } else {
    
    # Return NULL
    df <- response
    
  }
  
  # Return
  df
  
}
