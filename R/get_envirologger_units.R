#' Function to get units data from Envirologger API. 
#' 
#' @author Stuart K. Grange
#' 
#' @param user An Envirologger API user-name. 
#' 
#' @param key An Envirologger API key for \code{user}. 
#' 
#' @return Tibble. 
#' 
#' @seealso \href{https://api.airmonitors.net/3.5/documentation}{API Documentation}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get units table
#' get_envirologger_units(user, key)
#' 
#' }
#' 
#' @export
get_envirologger_units <- function(user, key) {
  
  # Build query
  query <- base_envirologger_url(user, key) %>% 
    stringr::str_c("units")
  
  # Get response
  response <- readLines(query, warn = FALSE)
  
  # Check
  response_check(response)
  
  # To tibble
  df <- response %>% 
    jsonlite::fromJSON() %>% 
    purrr::set_names(str_to_underscore(names(.))) %>% 
    as_tibble()
  
  return(df)
  
}
