#' Function to clean a data frame returned by \code{get_envirologger_data}. 
#' 
#' \code{clean_envirologger_data} creates a variable variable/column with help
#' of a look-up table and treats statuses as observations. 
#' 
#' @param df Data frame returned by \code{get_envirologger_data}. 
#' 
#' @seealso \code{\link{get_envirologger_data}}
#' 
#' @author Stuart K. Grange
#' 
#' @importFrom magrittr %>%
#' 
#' @export
clean_envirologger_data <- function(df) {
  
  # Load look-up table
  df_label_look_up <- read_label_look_up() %>% 
    dplyr::select(label, 
                  variable)
  
  # Make clean data
  df <- df %>% 
    dplyr::left_join(df_label_look_up, "label") %>% 
    dplyr::filter(!is.na(value), 
                  !is.na(label))
  
  # Do some checking
  if (any(is.na(df$variable))) 
    warning("Missing variables detected...", call. = FALSE)
  
  # Treat status codes as variables
  df_status <- df %>% 
    dplyr::select(date,
                  station, 
                  variable,
                  value = status) %>% 
    dplyr::mutate(variable = str_c("status_", variable))
  
  # Bind observations and status again and arrange
  df <- df %>% 
    dplyr::bind_rows(df_status) %>% 
    dplyr::select(-status) %>% 
    dplyr::arrange(date,
                   station,
                   variable) %>% 
    threadr::arrange_left(c("date", "station", "variable", "value"))

  # Return
  df
  
}
