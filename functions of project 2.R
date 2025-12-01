library(tidyverse)


data18 = read.csv(here::here("AQ_2018.csv"))
data19 = read.csv(here::here("AQ_2019.csv"))
data20 = read.csv(here::here("AQ_2020.csv"))


pollution_data = function(data) {
  data <- data |>
    rename( #renaming columns names, makes them more readable
      
      "CO_Daily8HRMAX" = Daily.Max.8.hour.CO.Concentration,
      "PM25_DailyMEAN" = Daily.Mean.PM2.5.Concentration,
      "Ozone_Daily8HRMAX" = Daily.Max.8.hour.Ozone.Concentration
    ) |>
    select(Date, CO_Daily8HRMAX, PM25_DailyMEAN, Ozone_Daily8HRMAX)
  
  
  #set date to right format
  
  data$Date = as.Date(data$Date, format = "%m/%d/%Y")
  
  
  #daily average for days with multiple measurements
  
  data = data |>
    group_by(Date) |>
    summarise(
      CO_Daily8HRMAX = mean(CO_Daily8HRMAX, na.rm = TRUE),
      PM25_DailyMEAN = mean(PM25_DailyMEAN, na.rm = TRUE),
      Ozone_Daily8HRMAX = mean(Ozone_Daily8HRMAX, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Date)
  
  #remove duplicate rows
  
  data = data |>
    distinct()
  
  #setting negative values to zero while looping through the pollutants
  
  pollutants <- c("CO_Daily8HRMAX", "PM25_DailyMEAN", "Ozone_Daily8HRMAX")
  for (pollutant in pollutants) {
    data[[pollutant]] <- ifelse(data[[pollutant]] < 0, 0, data[[pollutant]])
  }
  
  return(data)
}
  

d18 <- pollution_data(data18)
d19 <- pollution_data(data19)
d20 <- pollution_data(data20)

