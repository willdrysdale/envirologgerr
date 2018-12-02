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
#' @seealso \href{https://api.airmonitors.net/3.0/documentation}{API Documentation},
#' \code{\link{get_envirologger_data}}, \code{\link{get_envirologger_sensors}}
#' 
#' @examples 
#' \dontrun{
#' 
#' get_envirologger_sensors(user, key)
#' 
#' }
#' 
#' @export
get_envirologger_sensors <- function(user, key) {
  
  # Location
  base_url <- base_envirologger_url(user, key)
  
  # Build query
  query <- stringr::str_c(base_url, "sensors")
  
  # Get response
  response <- readLines(query, warn = FALSE)
  
  # Check
  response_check(response)
  
  # Parse
  df <- jsonlite::fromJSON(response)
  
  # Clean names
  names(df) <- str_underscore(names(df))
  
  # Lower case and trim the label variable
  df$label <- stringr::str_to_lower(df$label)
  df$label <- stringr::str_trim(df$label)
  
  # Arrange
  df <- df %>% 
    arrange(sensor_id) %>% 
    as_tibble()

  return(df)
  
}
