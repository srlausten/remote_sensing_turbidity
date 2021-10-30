# HEADER --------------------------------------------
#
# Author: Sam Lausten
# Email:  srlausten99@vt.edu
#
# Script Description:
# To explore the relation between light sensors and turbidity in McDonald's Hollow Stream - Blacksburg, VA


# INSTALL PACKAGES & LOAD LIBRARIES -----------------

#install.packages('tidyverse')
library(tidyverse)
#install.packages("dplyr") 
library(dplyr)
#install.packages('lubridate')
library(lubridate)
#install.packages('patchwork')
library(patchwork)

# SET WORKING DIRECTORY ---------------------------

# Starting at highest elevation and going down the stream

data_files <- list.files("~/Documents/Environmental_Informatics/git/remote_sensing_turbidity/data/sensor_data/")

# Iterating through sensor files

for(i in 1:length(data_files)) {                              
  assign(paste0("sensor", i),                                   
         read_csv(paste0("~/Documents/Environmental_Informatics/git/remote_sensing_turbidity/data/sensor_data/",
                         data_files[i])))
}

#--------- sensors of interest (preprocessing)


# date column function

date_func <- function(df){
  df %>% 
    mutate(month = month(DateTime),
           day = day(DateTime), 
           year = year(DateTime), 
           hour = hour(DateTime))
}

# adding date columns

sensor21 <- date_func(sensor21)
sensor22 <- date_func(sensor22)
sensor23 <- date_func(sensor23)
sensor24 <- date_func(sensor24)


# cleaning function to organize and filter data

cleaning_func <- function(df){
  df %>% 
    group_by(hour) %>%
    summarize(Int_lmft2 = mean(Int_lmft2)) %>% 
    filter(Int_lmft2 >= 0)
}

sensor21_mean <- cleaning_func(sensor21)
sensor22_mean <- cleaning_func(sensor22)
sensor23_mean <- cleaning_func(sensor23)
sensor24_mean <- cleaning_func(sensor24)




# plotting 2 sensors before mine and 2 after by hourly avg

plot_sensor21 <- ggplot(sensor21_mean, aes(x = hour, y = Int_lmft2))+
  geom_point()+ 
  labs(x = 'Hour' , y = 'Light Intensity(lum/ft2)', title = 'Sensor 21 Avg Light Intensity by Hour 
       of the Day')
plot_sensor21

plot_sensor22 <- ggplot(sensor22_mean, aes(x = hour, y = Int_lmft2))+
  geom_point()+ 
  labs(x = 'Hour' , y = 'Light Intensity(lum/ft2)', title = 'Sensor 22 Avg Light Intensity by Hour 
       of the Day')
plot_sensor22

plot_sensor23 <- ggplot(sensor23_mean, aes(x = hour, y = Int_lmft2))+
  geom_point()+ 
  labs(x = 'Hour' , y = 'Light Intensity(lum/ft2)', title = 'Sensor 22 Avg Light Intensity by Hour 
       of the Day')
plot_sensor23

plot_sensor24 <- ggplot(sensor24_mean, aes(x = hour, y = Int_lmft2))+
  geom_point()+ 
  labs(x = 'Hour' , y = 'Light Intensity(lum/ft2)', title = 'Sensor 22 Avg Light Intensity by Hour 
       of the Day')
plot_sensor24

(sensor22_plot + sensor23_plot) / (rain_plot + rain_plot)

#Rain data, using same dates as remote temp/light sensing data collection period.

rain_data <- ("~/Documents/Environmental_Informatics/git/remote_sensing_turbidity/data/rain_data/rain_data.csv")

rain_dat <- read_csv(rain_data) %>% 
  filter(DATE >= '2020-08-05' & DATE <= '2020-10-02') %>% 
  select(DATE, PRCP) %>% 
  filter(PRCP > 10)


# plot of sensor 22 and sensor 23 (before and after the mine site) with days that had > 10

sensor22_plot <- ggplot(sensor22, aes(x = DateTime, y = Int_lmft2))+ 
  geom_col(color = 'brown') +
  labs(title = 'Sensor 22' , x = 'Date', y = 'Light Intensity(lum/ft2)')+ 
  ylim(0, 1000)
theme(axis.text.x = element_text(angle = 45))

sensor23_plot <- ggplot(sensor23, aes(x = DateTime, y = Int_lmft2))+   geom_col(color = 'brown') +
  labs(title = 'Sensor 23' , x = 'Date', y = 'Light Intensity(lum/ft2)')+ 
  ylim(0, 1000)
theme(axis.text.x = element_text(angle = 45))

rain_days <- rain_dat %>% 
  filter(PRCP > 10)

rain_plot <- ggplot(rain_days, aes(x = DATE, y = PRCP)) + geom_col()
labs(x = 'Date', y = 'Precipitation(mm)') + 
  theme(axis.text.x = 
          element_text(angle = 45))

rain_plot

sensor22_plot + rain_plot


# preparing to match rain_days days with sensor light data

rainy_days <- ymd(c('2020-08-06', '2020-08-14', '2020-08-15','2020-08-16', '2020-08-22', '2020-08-24', '2020-08-26', '2020-08-29', '2020-08-30', '2020-08-31', '2020-09-01', '2020-09-02', '2020-09-10', '2020-09-11', '2020-09-18', '2020-09-25', '2020-09-26', '2020-09-29', '2020-09-30'))


rain_dry_func <- function(df){
  df %>% 
    mutate(day = mdy(paste(month(DateTime), day(DateTime), year(DateTime))))%>%
    mutate(hour = hour(DateTime)) %>% 
    filter(hour > 7 & hour < 17) %>% 
    mutate(rain = case_when(
      day %in% rainy_days ~ "rain", 
      !(day %in% rainy_days) ~ "dry"
    ))
}


sensor23_rain <-rain_dry_func(sensor23)
sensor23_rain

# daily boxplot code for light sensors rain vs dry days, color coded

sensor23_fordays %>% 
  ggplot(aes(x = as.factor(day), y = Int_lmft2, fill = rain))+ 
  geom_boxplot(outlier.shape = NA)+
  ylim(0,75)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(title = "Sensor 23", x = "8/5/2020 - 10/2/2020", y = "Light 
  Intensity (lum/ft2)") 


# comparing light sensors on consecutive days with rain 

start <- ymd("2020-09-25")
end <- ymd("2020-09-28")

rain_func <- function(df){
  df %>%   
    filter(DateTime >= start & DateTime <= end)
}



sensor21_rain <- rain_func(sensor21)
sensor22_rain <- rain_func(sensor22)
sensor23_rain <- rain_func(sensor23)
sensor24_rain <- rain_func(sensor24)


Dat_combined <- bind_rows("Sensor 24" = sensor24_rain,
                          "Sensor 23" = sensor23_rain, 
                          "Sensor 22" = sensor22_rain,
                          "Sensor 21" = sensor21_rain,
                          .id = "sensor_id")

Dat_combined %>% ggplot(aes(DateTime, Int_lmft2, color = sensor_id))+
  geom_line()+
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 50))+ 
  labs(title =  "Sensors 21-24 light intensity througout
       a rain event (9/26)", y = "Light Intensity(lum/ft2)", x = "Date")

