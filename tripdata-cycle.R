library(tidyverse)
library(scales)
library(janitor)
library(tidyr)

csv_files <- list.files("/Users/gokhanalphan/RStudioFile/Citybike", pattern = "*.csv", full.names = TRUE)

combined_data <- csv_files %>% lapply(read.csv) %>% bind_rows()

cleaned_cyclistic <- combined_data %>% 
  clean_names() %>% 
  mutate_all(~str_trim(.)) %>%
  mutate_all(~na_if(.,"")) %>% 
  drop_na() %>% 
  distinct()

## Prepare data; time differences column added, for the reliability - ride time between 100s to 10000s.
cyclistic <- cleaned_cyclistic %>% mutate(start_time = ymd_hms(started_at), end_time = ymd_hms(ended_at)) %>% 
  mutate(time_diff = as.duration(interval(start_time, end_time))) %>% filter(time_diff > 30) %>% 
  mutate(member_casual = str_to_title(member_casual)) %>%
  mutate(rideable_type = str_to_title(rideable_type)) %>%
  select(ride_id, rideable_type, start_station_name, start_station_id, end_station_name, end_station_id, member_casual, start_time, end_time, time_diff)

random_sampling <- cyclistic %>% slice_sample(prop = 0.05)


## AFTER CLEANING... Number of user types..
memberships2 <- cyclistic %>% count(member_casual, name = "Total") %>% 
  mutate(percentage = round(Total / sum(Total) * 100, 2))

ggplot(cyclistic, aes(x = member_casual, fill = member_casual)) + geom_bar() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Total Users in a year", x = "Member Types", y = "Total Numbers", fill = "Type of user") + theme(axis.text.x = element_blank())

## Which type of ride mostly used by casual or member users...
ggplot(cyclistic, aes(x = rideable_type, fill = member_casual)) + geom_bar() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = "Which type of bike mostly used", fill = "Types")

## MOST RIDES... 
casual_mostrides <- cyclistic %>% filter(member_casual == "Casual") %>%
  count(start_station_name, sort = TRUE, name = "most rides")

member_mostrides <- cyclistic %>% filter(member_casual == "Member") %>%
  count(start_station_name, sort = TRUE, name = "most rides")

## take top 5 used by membership...
most_rides <- cyclistic %>%
  count(member_casual, start_station_name, sort = TRUE, name = "total_rides") %>% 
  slice_head(n = 5)

## Overall users chart top 5...
print(cyclistic %>% count(start_station_name, sort = TRUE, name = "count_") %>% slice_head(n = 5))

most_used_five <- cyclistic %>% 
  filter(start_station_name == "Streeter Dr & Grand Ave" |
           start_station_name == "DuSable Lake Shore Dr & Monroe St" |
           start_station_name == "Michigan Ave & Oak St" |
           start_station_name == "DuSable Lake Shore Dr & North Blvd" |
           start_station_name == "Kingsbury St & Kinzie St")

## top 5 Riding Start locations.
ggplot(most_used_five, aes(y = start_station_name, fill = member_casual)) + geom_bar() +
  labs(title = "Most Used Start Points in a year", x = "How many times", y = "Station Names", fill = "Type of user") +
  theme_gray()

## Total Rides of top 7 routes

top_places <- cyclistic %>% 
  count(start_station_name, end_station_name, member_casual, name = "total", sort = TRUE) %>% 
  filter(total > 4050) %>% 
  unite(Destination, start_station_name, end_station_name, sep = " /\n")

ggplot(top_places, aes(x = total, y = reorder(Destination, total), fill = member_casual)) + geom_col() +
  labs(title = "Most Used Destinations in a year", x = "How many times used", y = "", fill = "Type of user")

## WHICH MEANS THESE 3 OUT OF TOP 5 DESTINATIONS LEAD US TO WHERE WE SHOULD START ADV. FOR CASUALS. 

## Time line for riding ratio in a year...

formated_month <- cyclistic %>% mutate(only_months = format(start_time, "%Y-%m")) %>% 
  count(only_months, member_casual, name = "Monthly use")

ggplot(formated_month, aes(x = only_months, y = `Monthly use`, group = member_casual, colour = member_casual)) + geom_line() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Bike Usage by month", colour = "Type of user") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) 

formated_hours <- cyclistic %>% mutate(only_hours = format(start_time, "%H")) %>% 
  count(only_hours, member_casual, name = "Hourly use")

ggplot(formated_hours, aes(x = only_hours, y = `Hourly use`, group = member_casual, colour = member_casual)) + geom_line() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Bike Usage by hour", colour = "Type of user") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

## When most used bikes? weekend or weekdays?

when_most_used <- cyclistic %>% mutate(day_type = ifelse(wday(start_time) %in% c(7, 1), "Weekend", "Weekday")) %>% 
  count(day_type, member_casual, name = "total")

ggplot(when_most_used, aes(x = day_type, y = total, fill = member_casual)) + geom_col() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  labs(title = "Weekday and Weekend Usage", fill = "Type of user") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

## Average riding periods between weekday and weekend..
mean_rides <- cyclistic %>% group_by(member_casual) %>% summarize(mean_times_min = mean(time_diff) / 60)

when_most_used2 <- cyclistic %>% mutate(day_type = ifelse(wday(start_time) %in% c(7, 1), "Weekend", "Weekday")) %>% 
  group_by(day_type, member_casual) %>% summarize(avg_riding = mean(time_diff) / 60, .groups = "drop")

ggplot(when_most_used2, aes(x = member_casual, y = avg_riding, fill = member_casual)) + geom_col() +
  facet_grid(~day_type) + labs(title = "Average Riding Durations", x = "", y = "Minutes", fill = "Type of user") +
  theme(axis.text.x = element_blank())

when_most_used3 <- cyclistic %>% mutate(day_type = wday(start_time, label = TRUE, abbr = FALSE)) %>% 
  count(member_casual ,day_type, name = "Total Rides")

ggplot(when_most_used3, aes(x = day_type, y = `Total Rides`, group = member_casual, colour = member_casual)) + geom_line() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + labs(title = "Bike Usage by day", colour = "Type of user") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

