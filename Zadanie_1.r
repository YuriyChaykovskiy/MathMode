library(tidyverse)
library(rnoaa)
library (dplyr)
library (lubridate)
station_data = ghcnd_stations()
#station_data = read_csv("station_data.csv")
vologda = data.frame(id = "VOLOGDA", latitude = 59.1326,  longitude = 39.5302)
vologda_around = meteo_nearby_stations(lat_lon_df = vologda, station_data = station_data,
                                    limit = 24, var = c("TAVG"),
                                    year_min = 1993, year_max = 2001)
vologda_id = vologda_around[["VOLOGDA"]][["id"]][1]
all_vologda_data = meteo_tidy_ghcnd(stationid = vologda_id)
all_vologda_data$date
class(all_vologda_data$date)
all_vologda_data$date+1
as.numeric(all_vologda_data$date)

all_vologda_data = all_vologda_data %>% mutate(
                  year = year(date),
                  month = month(date),
                  day = yday(date)
                  ) %>% filter(year > 1993 & year < 2001) %>% 
                  mutate(tavg = case_when(
                    is.na(tavg) ~ (tmax+tmin)/20, 
                    TRUE ~ tavg/10)) %>% select(year, month, day, tavg)

sum_act_temp = all_vologda_data %>% mutate(
                                      tavg = case_when(
                                            tavg < 5 ~ 0,
                                            is.na(tavg) ~ 0,
                                            TRUE ~ tavg)) 

sum_act_temp = sum_act_temp %>% group_by (month) %>% 
                     summarise(tavg = mean(tavg)) 
sum_act_temp = sum_act_temp %>%
  group_by(month) %>% summarise(St=sum(tavg)) 

Kf = 300; Qj = 1600; Lj = 2.2; Ej = 25;

yields = 10^6 * (((26.31+9.26*1*8.44)*1*Kf)/(Qj*Lj*(100-Ej)) + ((25.64+9.03*1*16.27)*1*Kf)/(Qj*Lj*(100-Ej)) + ((23.2+8.16*1*17.10)*1*Kf)/(Qj*Lj*(100-Ej)) + ((18.73+6.59*1*14.20)*0.32*Kf)/(Qj*Lj*(100-Ej)))
