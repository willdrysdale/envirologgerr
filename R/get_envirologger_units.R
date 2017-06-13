#' Function to get units data from Envirologger API. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user An Envirologger API user-name. 
#' 
#' @param key An Envirologger API key for \code{user}. 
#' 
#' @return Data frame.
#' 
#' @examples 
#' \dontrun{
#' 
#' get_envirologger_units(user, key)
#' 
#' }
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
  
  return(df)
  
}
