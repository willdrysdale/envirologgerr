#' Function to get available Envirologger API monitoring sensors.  
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
#' \code{\link{get_envirologger_data}}, \code{\link{get_envirologger_sensors}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get sensor information
#' get_envirologger_sensors(user, key)
#' 
#' }
#' 
#' @export
get_envirologger_sensors <- function(user, key) {
  
  # Build query
  query <- base_envirologger_url(user, key) %>% 
    stringr::str_c("sensors")
  
  # Get response
  response <- readLines(query, warn = FALSE)
  
  # Check
  response_check(response)
  
  # Parse and clean a bit
  df <- jsonlite::fromJSON(response) %>% 
    purrr::set_names(str_to_underscore(names(.))) %>% 
    arrange(label) %>% 
    as_tibble()

  return(df)
  
}
