#' Function to get units-data from Envirologger API. 
#' 
#' @param user Envirologger API user-name. 
#' @param key Envirologger API key for \code{user}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_envirologger_units <- function(user, key) {
  
  # Location
  base_url <- base_envirologger_url(user, key)
  
  # Build query
  query <- stringr::str_c(base_url, "units")
  
  # Get response
  response <- readLines(query, warn = FALSE)
  
  # Check
  response_check(response)
  
  # Parse
  df <- jsonlite::fromJSON(response)
  
  # Clean names
  names(df) <- str_underscore(names(df))
  
  # Return
  df
  
}
