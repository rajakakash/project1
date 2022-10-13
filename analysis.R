library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("C:/Users/akash/Desktop/data_case")
m1_2021 <- read_csv("202101-divvy-tripdata.csv")
m2_2021 <- read_csv("202102-divvy-tripdata.csv")
m3_2021 <- read_csv("202103-divvy-tripdata.csv")
m4_2021 <- read_csv("202104-divvy-tripdata.csv")
m5_2021 <- read_csv("202105-divvy-tripdata.csv")
m6_2021 <- read_csv("202106-divvy-tripdata.csv")
m7_2021 <- read_csv("202107-divvy-tripdata.csv")
m8_2021 <- read_csv("202108-divvy-tripdata.csv")
m9_2021 <- read_csv("202109-divvy-tripdata.csv")
m10_2021 <- read_csv("202110-divvy-tripdata.csv")
m11_2021 <- read_csv("202111-divvy-tripdata.csv")
m12_2021 <- read_csv("202112-divvy-tripdata.csv")
colnames(m1_2021)
colnames(m2_2021)
colnames(m3_2021)
colnames(m4_2021)
colnames(m5_2021)
colnames(m6_2021)
colnames(m7_2021)
colnames(m8_2021)
colnames(m9_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)
str(m1_2021)
str(m2_2021)
str(m3_2021)
str(m4_2021)
str(m5_2021)
str(m6_2021)
str(m7_2021)
str(m8_2021)
str(m9_2021)
str(m10_2021)
str(m11_2021)
str(m12_2021)
m1_2021 <-  mutate(m1_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m2_2021 <-  mutate(m2_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m3_2021 <-  mutate(m3_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m4_2021 <-  mutate(m4_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m5_2021 <-  mutate(m5_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m6_2021 <-  mutate(m6_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m7_2021 <-  mutate(m7_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m8_2021 <-  mutate(m8_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m9_2021 <-  mutate(m9_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m10_2021 <-  mutate(m10_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m11_2021 <-  mutate(m11_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m12_2021 <-  mutate(m12_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type))
all_trips <- bind_rows(m1_2021, m2_2021, m3_2021, m4_2021, m5_2021, m6_2021, m7_2021, m8_2021, m9_2021, m10_2021, m11_2021, m12_2021)
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(all_trips)
nrow(all_trips) 
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
table(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
summary(all_trips_v2$ride_length)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = "C:\\Users\\akash\\Desktop\\analysis\\avg_ride_length.csv")

