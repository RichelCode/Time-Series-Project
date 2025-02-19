
## Delay dataset

library(tidyverse)

delay <- read_csv('Airline_Delay_Cause.csv')

## Create city and state columns
delay <- delay %>%
  separate(airport_name, into = c("city", "state"), sep = ", ", extra = "merge")
delay <- delay %>%
  mutate(state = sub(":.*", "", state))

airport_delay_summary <- delay %>%
  group_by(airport, year, month) %>%
  summarise(total_arrivals = sum(arr_flights, na.rm = TRUE),
            #total_delay_arrival = sum(arr_delay, na.rm = TRUE),
            total_delay_15 = sum(arr_del15, na.rm = TRUE), 
            total_cancel = sum(arr_cancelled, na.rm = TRUE), .groups = "drop" )

airport_delay_summary <- airport_delay_summary %>%
  mutate(time_period = paste(year, month, sep = "-"))


all_combinations <- delay %>%
  distinct(airport) %>% # Get unique airports
  expand(airport, year = seq(min(delay$year), max(delay$year)), month = 1:12)

airport_delay_filled <- all_combinations %>%
  left_join(airport_delay_summary, by = c("airport", "year", "month")) %>%
  mutate(total_arrivals = ifelse(is.na(total_arrivals), 0, total_arrivals)) %>%
  #mutate(total_delay_arrival = ifelse(is.na(total_delay_arrival), 0, total_delay_arrival)) %>%
  mutate(total_delay_15 = ifelse(is.na(total_delay_15), 0, total_delay_15)) %>%
  mutate(total_cancel = ifelse(is.na(total_cancel), 0, total_cancel)) %>%
  mutate(time_period = paste(year, month, sep = "-"))


airport_delay_filled <- airport_delay_filled %>%
  filter(!(year == 2013 & month < 8) & !(year == 2023 & month > 8))

# Add state and city columns
airport_location_mapping <- delay %>%
  distinct(airport, city, state)

airport_delay_filled <- airport_delay_filled %>%
  left_join(airport_location_mapping, by = "airport")


## Plot some series
library(ggplot2)
abe_data <- airport_delay_filled %>%
  filter(airport == "ABE")

abe_data <- abe_data %>%
  mutate(time_period = as.Date(paste0(time_period, "-01")))

ggplot(abe_data, aes(x = time_period, y = total_arrivals)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Date",
       y = "Total Arrivals") +
  theme_minimal() 
