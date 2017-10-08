library(dplyr)
location <- read.csv("data/location.csv", stringsAsFactors = FALSE)
files <- location$file_name

charDate <- function(csv){
  csv <- read.csv(paste0("data/",csv,".csv"), stringsAsFactors = FALSE)
  csv$date <- as.Date(csv$date, "%Y-%m-%d")
  return(csv)
}

list.data <- lapply(files, charDate)

for (i in seq(list.data)) {
  assign(paste(files[i], sep = ""), list.data[[i]])
}
  



tempDates <- function(cal.date){
  kcltTemp <- kclt %>% filter(date == cal.date)
  print(kcltTemp$actual_mean_temp)
  
  return(4)
}

tempDates("2014-07-01")