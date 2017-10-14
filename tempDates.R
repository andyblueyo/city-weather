library(dplyr)

tempDates <- function(cal.date){
  location <- read.csv("data/location.csv", stringsAsFactors = FALSE)
  files <- location$file_name
  
  # convert csv files with appropriate dates
  charDate <- function(csv){
    csv <- read.csv(paste0("data/",csv,".csv"), stringsAsFactors = FALSE)
    csv$date <- as.Date(csv$date, "%Y-%m-%d")
    return(csv)
  }
  
  list.data <- lapply(files, charDate)
  for (i in seq(list.data)) {
    list.data[[i]] <- list.data[[i]] %>% filter(list.data[[i]]$date == cal.date)
    location$actual_mean_temp[location$file_name == files[i]] <<- list.data[[i]]$actual_mean_temp
  }

}

