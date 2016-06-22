#' @export
read_label_look_up <- function() {
  
  read.csv(file.path(system.file("extdata", package = "envirologgerr"), 
                     "envirologger_label_look_up.csv"), stringsAsFactors = FALSE)
  
}
