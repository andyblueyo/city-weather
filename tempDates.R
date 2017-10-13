library(dplyr)
location <- read.csv("data/location.csv", stringsAsFactors = FALSE)
files <- location$file_name


charDate <- function(csv){
  csv <- read.csv(paste0("data/",csv,".csv"), stringsAsFactors = FALSE)
  csv$date <- as.Date(csv$date, "%Y-%m-%d")
  return(csv)
}

list.data <- lapply(files, charDate)

#read in all the files
for (i in seq(list.data)) {
  assign(paste(files[i], sep = ""), list.data[[i]])
}
  
for (i in seq(list.data)) {
  list.data[[i]] <- list.data[[i]] %>% filter(list.data[[i]]$date == "2014-07-06")
  #assign(paste(files[i], sep = ""), list.data[[i]])
  location$actual_mean_temp[location$file_name == files[i]] <- list.data[[i]]$actual_mean_temp
}

location$actual_mean_temp[location$file_name == "KCLT"] <- KCLT[["actual_mean_temp"]]

tempDates <- function(cal.date){
  kcltTemp <- KCLT %>% filter(date == cal.date)
  print(kcltTemp)

}

tempDates("2014-07-06")