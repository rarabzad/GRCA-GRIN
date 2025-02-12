library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

get_station_list <- function()
{
  base_url <- "https://waterdata.grandriver.ca/KiWIS/KiWIS?"
  
  url_stations <- paste0(base_url,
                         "service=kisters&type=queryServices&request=getStationList",
                         "&datasource=0&format=csv")
  
  response <- GET(url_stations)
  raw_text <- content(response, "text")
  
  # Read CSV with proper delimiter
  station_data <- read_delim(raw_text, show_col_types = FALSE, delim = ";")
  
  # Ensure valid response
  if (nrow(station_data) == 0) {
    stop("Error: No stations found.")
  }
  
  return(station_data)
}
get_station_data <- function(station_no, start_date, end_date)
{
  library(httr)
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  
  base_url <- "https://waterdata.grandriver.ca/KiWIS/KiWIS?"
  
  # Convert dates to Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Step 1: Get the list of time series for the station
  url_ts_list <- paste0(base_url,
                        "service=kisters&type=queryServices&request=getTimeseriesList",
                        "&datasource=0&format=csv&station_no=", station_no)
  
  response_ts_list <- GET(url_ts_list)
  raw_ts_text <- content(response_ts_list, "text")
  
  # Read CSV with proper delimiter
  ts_data <- read_delim(raw_ts_text, show_col_types = FALSE, delim = ";")
  
  # Ensure valid response
  if (nrow(ts_data) == 0 || !"ts_id" %in% colnames(ts_data)) {
    stop("Error: No valid time series data found for station.")
  }
  
  # Step 2: Retrieve data for each parameter over multiple years
  param_data_list <- list()
  
  for (i in seq_len(nrow(ts_data))) {
    ts_id <- ts_data$ts_id[i]
    param_name <- ts_data$parametertype_name[i]
    
    # Initialize empty tibble to store all years' data
    full_data <- tibble(Timestamp = as.POSIXct(character()), Value = numeric())
    
    # Loop through each year
    for (year in seq(year(start_date), year(end_date))) {
      from_date <- as.character(as.Date(paste0(year, "-01-01")))
      to_date <- as.character(as.Date(paste0(year, "-12-31")))
      
      # Adjust end date if it's the last year
      if (year == year(end_date)) {
        to_date <- as.character(end_date)
      }
      
      # Construct API URL for this year
      url_data <- paste0(base_url,
                         "service=kisters&type=queryServices&request=getTimeseriesValues",
                         "&datasource=0&format=csv",
                         "&ts_id=", ts_id,
                         "&from=", from_date,
                         "&to=", to_date)
      
      response_data <- GET(url_data)
      raw_data_text <- content(response_data, "text")
      
      # Read data with proper delimiter and skip metadata rows
      yearly_data <- read_delim(raw_data_text, show_col_types = FALSE, delim = ";", skip = 2, col_names = c("Timestamp", "Value"))
      
      # Clean and convert the 'Value' column
      yearly_data <- yearly_data %>%
        mutate(
          Timestamp = parse_date_time(Timestamp, orders = c("Ymd HMS", "Ymd_HMS", "Y-m-d H:M:S"), quiet = TRUE),
          Value = str_trim(Value),  # Trim whitespace
          Value = str_replace_all(Value, "[^0-9.-]", ""),  # Remove non-numeric characters
          Value = as.numeric(Value)  # Convert to numeric
        ) %>%
        drop_na()  # Remove any NA values caused by parsing errors
      
      # Append yearly data
      full_data <- bind_rows(full_data, yearly_data)
    }
    
    # Store final dataset in the list
    param_data_list[[param_name]] <- full_data
  }
  
  return(param_data_list)
}
sanitize_filename <- function(filename)
{
  # Replace all invalid characters with an underscore "_"
  filename <- gsub("[/\\\\?%*:|\"<>]", "_", filename)
  
  # Trim leading and trailing spaces
  filename <- trimws(filename)  
  
  return(filename)
}

# Example usage
station_list <- get_station_list()
start_date <-"1980-01-01"
end_date <- "2024-12-30"
for(i in 74:nrow(station_list))
{
  data<-
    tryCatch({
      get_station_data(station_no = as.character(station_list$station_no[i]),
                       start_date = start_date,
                       end_date = end_date)
    }, error = function(e) {
      return(NULL)  # Return NULL if an error occurs
    })
  if(!is.null(data))
  {
    for(j in 1:length(data))
    {
      if(nrow(data[[j]])>0)
      {
        file <- file.path(getwd(),sanitize_filename(sprintf("%s (%s).csv",station_list$station_name[i],names(data)[j])))
        write.csv(data[[j]],file = file,row.names = F)
      }
    }
  }
}
