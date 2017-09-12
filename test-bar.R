#install.packages("viridis") # dependency
#install.packages("devtools")
#devtools::install_github("ropensci/plotly")

library(dplyr)
library(plotly)
library(ggplot2)

weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)
temp <- weather %>% filter(actual_max_temp > 50) %>% select(actual_min_temp, actual_max_temp, actual_mean_temp, date)

pls <- ggplot(temp, aes(x = `date`, ymin =`actual_min_temp`, ymax = `actual_max_temp`, middle = `actual_max_temp`, upper = `actual_max_temp`, lower = `actual_min_temp`))+
  geom_boxplot(stat = 'identity') +
  xlab('date') + 
  ylab('Temperature')
pls
oh <- ggplotly(pls)
oh