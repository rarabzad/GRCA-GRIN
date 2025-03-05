# Water Data Retrieval from Grand River API

## Overview
This repo provides a set of R functions to retrieve water data from the Grand River API. The script allows users to:
- Fetch a list of available stations.
- Retrieve time series data for a specified station over multiple years.
- Save the data in CSV format with sanitized filenames.
- Handle errors gracefully to ensure robust data retrieval.

## Prerequisites
Ensure you have the following R packages installed:
```r
install.packages(c("httr", "readr", "dplyr", "tidyr", "lubridate", "stringr"))
```
then load the packages:
```r
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
```

## Functions

### 1. `get_station_list()`
Retrieves the list of available stations from the Grand River API.

#### Usage:
```r
station_list <- get_station_list()
```

#### Implementation:
```r
get_station_list <- function() {
  base_url <- "https://waterdata.grandriver.ca/KiWIS/KiWIS?"
  url_stations <- paste0(base_url, "service=kisters&type=queryServices&request=getStationList", "&datasource=0&format=csv")
  response <- GET(url_stations)
  raw_text <- content(response, "text")
  station_data <- read_delim(raw_text, show_col_types = FALSE, delim = ";")
  
  if (nrow(station_data) == 0) {
    stop("Error: No stations found.")
  }
  return(station_data)
}
```

---
### 2. `get_station_data(station_no, start_date, end_date)`
Retrieves time series data for a specific station within a given date range.
The API only allows retrieving one year of data per request, so the function loops through multiple years and appends the data.

#### Parameters:
- `station_no` : Station number
- `start_date` : Start date in YYYY-MM-DD format
- `end_date` : End date in YYYY-MM-DD format

#### Usage:
```r
data <- get_station_data(station_no = "144", start_date = "1980-01-01", end_date = "2024-12-30")
```

#### Implementation:
```r
get_station_data <- function(station_no, start_date, end_date) {
  base_url <- "https://waterdata.grandriver.ca/KiWIS/KiWIS?"
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  url_ts_list <- paste0(base_url, "service=kisters&type=queryServices&request=getTimeseriesList", "&datasource=0&format=csv&station_no=", station_no)
  response_ts_list <- GET(url_ts_list)
  raw_ts_text <- content(response_ts_list, "text")
  ts_data <- read_delim(raw_ts_text, show_col_types = FALSE, delim = ";")
  
  if (nrow(ts_data) == 0 || !"ts_id" %in% colnames(ts_data)) {
    stop("Error: No valid time series data found for station.")
  }
  
  param_data_list <- list()
  for (i in seq_len(nrow(ts_data))) {
    ts_id <- ts_data$ts_id[i]
    param_name <- ts_data$parametertype_name[i]
    full_data <- tibble(Timestamp = as.POSIXct(character()), Value = numeric())
    
    for (year in seq(year(start_date), year(end_date))) {
      from_date <- as.character(as.Date(paste0(year, "-01-01")))
      to_date <- ifelse(year == year(end_date), as.character(end_date), as.character(as.Date(paste0(year, "-12-31"))))
      
      url_data <- paste0(base_url, "service=kisters&type=queryServices&request=getTimeseriesValues", "&datasource=0&format=csv", "&ts_id=", ts_id, "&from=", from_date, "&to=", to_date)
      response_data <- GET(url_data)
      raw_data_text <- content(response_data, "text")
      yearly_data <- read_delim(raw_data_text, show_col_types = FALSE, delim = ";", skip = 2, col_names = c("Timestamp", "Value"))
      
      yearly_data <- yearly_data %>%
        mutate(
          Timestamp = parse_date_time(Timestamp, orders = c("Ymd HMS", "Ymd_HMS", "Y-m-d H:M:S"), quiet = TRUE),
          Value = str_trim(Value),
          Value = str_replace_all(Value, "[^0-9.-]", ""),
          Value = as.numeric(Value)
        ) %>% drop_na()
      
      full_data <- bind_rows(full_data, yearly_data)
    }
    param_data_list[[param_name]] <- full_data
  }
  return(param_data_list)
}
```

---
### 3. `sanitize_filename(filename)`
Sanitizes filenames by replacing invalid characters with underscores.

#### Usage:
```r
clean_name <- sanitize_filename("Sulphur Creek d/s fish ladder (HG).csv")
```

#### Implementation:
```r
sanitize_filename <- function(filename) {
  filename <- gsub("[/\\?%*:|\"<>]", "_", filename)
  filename <- trimws(filename)  
  return(filename)
}
```

---
### 4. Full Workflow
This script retrieves station data, handles errors, and writes output to CSV files.

```r
station_list <- get_station_list()
start_date <- "1980-01-01"
end_date <- "2024-12-30"

for(i in 1:nrow(station_list)) {
  data <- tryCatch({
    get_station_data(station_no = as.character(station_list$station_no[i]), start_date = start_date, end_date = end_date)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(data)) {
    for (j in 1:length(data)) {
      if (nrow(data[[j]]) > 0) {
        file <- file.path(getwd(), sanitize_filename(sprintf("%s (%s).csv", station_list$station_name[i], names(data)[j])))
        write.csv(data[[j]], file = file, row.names = FALSE)
      }
    }
  }
}
```

This script reads station data, handles the missing values by putting blank rows in the data, and writes output to CSV files.

```r
library(lubridate)
library(xts)
files <- list.files(pattern = "*.csv")
data <- lapply(files, read.csv)
names(data) <- gsub(".csv", "", files)

for (i in 1:length(data))
{
  timestamps <- ymd_hms(data[[i]]$Timestamp)
  time_diffs <- diff(timestamps)  # Returns difftime object
  mode_diff <- as.numeric(names(sort(table(as.numeric(time_diffs)), decreasing = TRUE)[1]))  # Find mode
  interval_unit <- units(time_diffs)  # e.g., "secs", "mins", "hours"
  full_time_seq <- seq(from = min(timestamps), to = max(timestamps), by = paste(mode_diff, interval_unit))
  xts_data <- xts(x = data[[i]]$Value, order.by = timestamps)
  filled_data <- merge(xts_data, xts(, order.by = full_time_seq), all = TRUE)
  df_out <- data.frame(Timestamp = index(filled_data), Value = coredata(filled_data))
  write.csv(df_out, paste0(names(data)[i], "_blank_rows_added.csv"), row.names = FALSE)
}
```

## Conclusion
This script provides a robust and automated way to retrieve historical water data. It efficiently handles multiple-year requests, sanitizes filenames, and ensures data integrity by gracefully handling API errors.

Feel free to contribute, report issues, or improve this workflow! 🚀

