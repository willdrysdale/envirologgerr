#' Function to get available Envirologger API monitoring stations for a user. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user An Envirologger API user-name. 
#' 
#' @param key An Envirologger API key for \code{user}. 
#' 
#' @return Tibble. 
#' 
#' @seealso \href{https://api.airmonitors.net/3.5/documentation}{API Documentation},
#' \code{\link{get_envirologger_data}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get station information
#' get_envirologger_stations(user, key)
#' 
#' }
#' 
#' @export
get_envirologger_stations <- function(user, key) {
  
  # Build query
  query <- base_envirologger_url(user, key) %>% 
    stringr::str_c("devices")
  
  # Get response
  response <- readLines(query, warn = FALSE)
  
  # Check
  response_check(response)
  
  # Parse and clean table a bit
  df <- jsonlite::fromJSON(response) %>% 
    purrr::set_names(str_to_underscore(names(.))) %>% 
    rename(station = unique_id) %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           altitude = as.numeric(altitude)) %>% 
    arrange(station) %>% 
    as_tibble()
  
  return(df)
  
}
