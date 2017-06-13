#' Function to read Envirologger XML documents. 
#' 
#' \code{read_envirologger_xml} is not fast and is not suitable for large XML 
#' documents. 
#' 
#' @seealso \code{\link{read_envirologger_json}}
#' 
#' @param file File name of Envirologger XML file. 
#' 
#' @param tz Time-zone of dates within \code{file}. Default is \code{"UTC"}. 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Load a file
#' data_test <- read_envirologger_xml("bt_tower_data.xml")
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.
#' 
#' @export
read_envirologger_xml <- function(file, tz = "UTC") {
  
  # Load file as text
  text <- readLines(file, warn = FALSE)
  
  # Make a list
  list_xml <- XML::xmlToList(text)
  
  # Extract vectors
  serial_number <- list_xml$Station$SerialNumber
  site <- list_xml$Station$Name
  
  # Get observations
  df <- list_xml$Station$Dynamics
  
  # Make a data frame
  df <- lapply(df, data.frame)
  df <- plyr::rbind.fill(df)
  
  # Clean
  df <- clean_envirologger_observations(df, serial_number, site, tz)
  
  # Return
  df
  
}


#' Function to read Envirologger XML documents which have been converted to JSON. 
#' 
#' \code{read_envirologger_json} is faster than the XML version and is suitable
#' for much larger file sizes. 
#' 
#' @seealso \code{\link{read_envirologger_xml}}
#' 
#' @param file File name of Envirologger JSON file. 
#' @param tz Time-zone of dates within \code{file}. Default is \code{"UTC"}. 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Load a file
#' data_test <- read_envirologger_json("bt_tower_data.json")
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_envirologger_json <- function(file, tz = "UTC") {
  
  # Load file as text
  text <- readLines(file, warn = FALSE)
  
  # Parse document
  json <- jsonlite::fromJSON(text)
  
  # Extract vectors
  serial_number <- json$AirQuality$Station$SerialNumber
  site <- json$AirQuality$Station$Name
  
  # Get observations
  df <- json$AirQuality$Station$Dynamics$AirQualityDynamic
  
  # Clean
  df <- clean_envirologger_observations(df, serial_number, site, tz)
  
  # Return
  df
  
}


# No export
clean_envirologger_observations <- function(df, serial_number, site, tz) {
  
  # Clean names
  names(df) <- stringr::str_to_lower(names(df))
  names(df) <- ifelse(names(df) == "datetime", "date", names(df))
  names(df) <- stringr::str_replace(names(df), "_ppb", "")
  
  # Add vectors
  df$serial_number <- serial_number
  df$site <- site
  
  # Data type work
  df$date <- lubridate::dmy_hm(df$date, tz = tz)
  
  df[, -1] <- lapply(df[, -1], function(x) 
    type.convert(as.character(x), as.is = TRUE))
  
  # NA catch
  df[, -1] <- lapply(df[, -1], function(x) ifelse(x == -999, NA, x))
  
  # Return
  df
  
}
