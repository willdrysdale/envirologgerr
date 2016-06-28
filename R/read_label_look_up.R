#' Function to load look-up table for the Envirologger label and sensor names. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame with correct data types.
#' 
#' @examples 
#' \dontrun{
#' 
#' # Load look-up table
#' data_labels <- read_label_look_up()
#' 
#' }
#' 
#' @export
read_label_look_up <- function() {
  
  # Load data
  df <- read.csv(file.path(system.file("extdata", package = "envirologgerr"), 
                     "envirologger_label_look_up.csv"), stringsAsFactors = FALSE)
  
  # Transform
  df[] <- lapply(df[], function(x) ifelse(x == "", NA, x))
  df$drop <- ifelse(is.na(df$drop), FALSE, df$drop)
  df$variable <- ifelse(is.na(df$variable), df$label, df$variable)
  
  # Return
  df
  
}
