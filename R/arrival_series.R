

library(tidyverse)
library(fable)
library(forecast)
airport_traffic <- read_csv("usa.csv")

airport_traffic_summary <- airport_traffic %>%
  group_by(airport_2, Year, quarter) %>%
  summarise(total_passengers = sum(passengers, na.rm = TRUE),
           total_fare = sum(fare, na.rm = TRUE), .groups = "drop" )

airport_traffic_summary <- airport_traffic_summary %>%
  mutate(time_period = paste(Year, "Q", quarter, sep = "-"))


all_combinations <- airline_traffic %>%
  distinct(airport_2) %>% # Get unique airports
  expand(airport_2, Year = seq(min(airport_traffic$Year), max(airport_traffic$Year)), quarter = 1:4)

airport_traffic_filled <- all_combinations %>%
  left_join(airport_traffic_summary, by = c("airport_2", "Year", "quarter")) %>%
  mutate(total_passengers = ifelse(is.na(total_passengers), 0, total_passengers)) %>%
  mutate(total_fare = ifelse(is.na(total_fare), 0, total_fare)) %>%
  mutate(time_period = paste(Year, "Q", quarter, sep = "-"))



