#' Function to get data from Envirologger API. 
#' 
#' The Envirologger API is not fast. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user Envirologger API user-name. 
#' @param key Envirologger API key for \code{user}. 
#' @param station Station's data to download. 
#' @param server API server to use for download. 
#' @param start Start date of query.
#' @param end End date of query. 
#' @param tz Time-zone for dates.
#' @param extra Should the returned data frame contain extra infomation? Default
#' is \code{TRUE} but setting to \code{FALSE} can be useful for packages such
#' as \strong{openair}. 
#' 
#' @seealso \href{http://api.envirologger.net/2.0/documentation}{Documentation},
#' \code{\link{get_envirologger_stations}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get some data
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
                                  tz = "UTC", extra = TRUE) {
  
  # Parse arguments
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Create day sequence, but will be strings
  df_dates <- data.frame(date = seq(start, end, "day")) %>% 
    mutate(date_end = date + lubridate::days(1),
           date_end = date_end - 1,
           date = str_replace_all(date, "-|:| ", ""), 
           date_end = str_replace_all(date_end, "-|:| ", ""))
  
  # Build query strings
  query <- str_c("stationdata/bydate/", server, "/", station, "/", df_dates$date,
                 "/", df_dates$date_end)
  
  # Add base url
  query <- str_c(base_envirologger_url(user, key), query)
  
  # Get data
  df <- data.frame(query = query) %>%
    rowwise() %>%
    do(get_data_worker(url = .$query, tz = tz)) %>%
    ungroup()
  
  # df <- plyr::ldply(query, get_data_worker, tz = tz)
  
  if (!nrow(df) == 0) {
    
    # Clean names
    names(df) <- str_underscore(names(df))
    names(df) <- ifelse(names(df) == "pre_scaled", "value", names(df))
    
    # Lower case
    df$label <- str_to_lower(df$label)
    
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


# Function to get read json return and format into a data frame 
#
# No export
get_data_worker <- function(url, tz) {
  
  # Get station from url
  station <- str_split_fixed(url, "/", 13)[, 11]
  station <- as.integer(station)
  
  # Get response
  df <- tryCatch({
    
    suppressWarnings(
      response <- readLines(url, warn = FALSE)
    )
    
  }, error = function(e) {
    
    warning("No data returned.", call. = FALSE)
    NULL
    
  })
  
  # If we get a return
  if (!is.null(df)) {
    
    # Check
    response_check(response)
    
    # Parse
    response <- fromJSON(response)
    
    # Get date
    date <- response$Timestamp
    # Parse
    date <- lubridate::ymd_hms(date, tz = tz)
    
    # Get observations
    df <- response$Channels
    
    # Insert date into observations
    df <- mapply(cbind, df, "date" = date, SIMPLIFY = FALSE)
    
    # Create data frame
    df <- bind_rows(df)
    
    # Add station key
    df$station <- station
    
  }
  
  # Return
  df
  
}
