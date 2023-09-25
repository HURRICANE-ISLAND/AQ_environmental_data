#HOBO Logger Plotting#
library(tidyverse)
library(lubridate)
library(ggpubr)
library(dbplyr)

#2019#

hobo.2019 <- read_csv("~/Desktop/WD/GSI/HOBO/HOBO.2019.csv")


hobo.2019$DateTime <- as.POSIXct(hobo.2019$DateTime,format="%Y-%m-%d %H:%M:%S")
str(hobo.2019)
view(hobo.2019)
ggplot(hobo.2019, aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(alpha = 0.2, color = "orange") +
  ggtitle("2019 raw HOBO data")

avg.hobo.2019 <- hobo.2019 %>% mutate(date = floor_date(DateTime, "day")) %>%
  group_by(date, Site) %>% summarize(mean_temp_c = mean(`Temperature (C)`), mean_temp_f = mean(`Temperature (F)`))

avg.hobo.2019 %>% filter(mean_temp_f < 60) %>% 
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_line() + 
  geom_smooth(method = "loess") +
  xlim(as.POSIXct(c("2019-06-28", "2020-05-10"))) +
  xlab("Date") + ylab("mean temperature (C)") +
  ggtitle("HI, Summer 2019-Spring 2020 Data")


avg.hobo.2019 %>% filter(mean_temp_f < 60) %>% 
  ggplot(aes(x = date, y = mean_temp_f)) +
  geom_line(aes(color = Site)) + 
  geom_smooth(method = "loess") +
  xlim(as.POSIXct(c("2019-06-28", "2020-05-10"))) +
  xlab("Date") + ylab("mean temperature (F)")

avg.hobo.2019 <- avg.hobo.2019 %>%
  filter(mean_temp_f <60) %>% filter(date < as.Date("2020-05-10"))

avg.hobo.2019 %>% 
  ggplot(aes(x = date, y = mean_temp_f)) +
  geom_line(aes(color = Site)) + 
  geom_smooth(method = "loess") +
  xlab("Date") + ylab("mean temperature (F)")

#2020#
hobo.2020 <- read_csv("~/Desktop/WD/GSI/HOBO/HOBO.2020.csv")
hobo.2020$DateTime <- as.POSIXct(hobo.2020$DateTime,format="%Y-%m-%d %H:%M:%S")
str(hobo.2020)
view(hobo.2020)
ggplot(hobo.2020, aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(alpha = 0.2, aes(color = Site)) + ggtitle("2020 raw HOBO data")

avg.hobo.2020 <- hobo.2020 %>% mutate(date = floor_date(DateTime, "day")) %>%
  group_by(date, Site) %>% summarize(mean_temp_c = mean(`Temperature (C)`), mean_temp_f = mean(`Temperature (F)`))

avg.hobo.2020%>% 
  ggplot(aes(x = date, y = mean_temp_f)) +
  geom_line() + 
  geom_smooth(method = "loess")  +
  xlab("Date") + ylab("mean temperature (F)") +
  xlim(as.POSIXct(c("2020-06-22", "2020-10-01"))) +
  ggtitle("All 2020 data")


avg.hobo.2020 %>% 
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_line(aes(color = Site)) + 
  xlim(as.POSIXct(c("2020-06-22", "2020-09-25"))) +
  xlab("Date") + ylab("mean temperature (C)")+
  ggtitle("mean temperature (C) 2020")

avg.hobo.2020 <- avg.hobo.2020 %>% filter(date < as.Date("2020-09-25"))

#2021#
hobo.2021 <- read_csv("~/Desktop/WD/GSI/HOBO/HOBO.2021.csv")
hobo.2021$DateTime <- as.POSIXct(hobo.2021$DateTime,format="%Y-%m-%d %H:%M:%S")
str(hobo.2021)
view(hobo.2021)
ggplot(hobo.2021, aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(alpha = 0.2, color = "orange") + ggtitle("2021 raw HOBO data")

hobo.2021 %>% filter(Number != 16) %>% 
  ggplot(aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(aes(color = Site), alpha = 0.2) + ggtitle("2021 raw HOBO data")

hobo.2021 %>% filter(Site == "NH") %>% filter(Number == "16")

avg.hobo.2021 <- hobo.2021 %>%  
  filter(Number != 16) %>%
  mutate(date = floor_date(DateTime, "day")) %>%
  group_by(date, Site) %>% summarize(mean_temp_c = mean(`Temperature (C)`), mean_temp_f = mean(`Temperature (F)`))


avg.hobo.2021 %>%
  ggplot(aes(x = date, y = mean_temp_f)) +
  geom_line() + 
  geom_smooth(method = "loess")  +
  xlab("Date") + ylab("mean temperature (F)") +
  ggtitle("")


avg.hobo.2021 %>% 
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_line(aes(color = Site)) + 
  xlab("Date") + ylab("mean temperature (C)") +
  xlim(as.POSIXct(c("2021-07-06", "2021-10-10"))) +
  ggtitle("mean temperature (C) 2021")

avg.hobo.2021 <- avg.hobo.2021 %>% filter(date < as.Date("2021-10-10"))

#2022#
hobo.2022 <- read_csv("~/Desktop/WD/GSI/HOBO/HOBO.2022.csv")
hobo.2022$DateTime <- as.POSIXct(hobo.2022$DateTime,format="%Y-%m-%d %H:%M:%S")
str(hobo.2022)
view(hobo.2022)
ggplot(hobo.2022, aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(alpha = 0.2, color = "orange") +ggtitle("2022 raw HOBO data")

hobo.2022 %>% filter(Number != 16) %>% 
  ggplot(aes(x = DateTime, y = `Temperature (C)`)) + 
  geom_point(aes(color = Site), alpha = 0.2) + ggtitle("2022 raw HOBO data")

hobo.2022 %>% filter(Site == "NH") %>% filter(Number == "16")

avg.hobo.2022 <- hobo.2022 %>%  
  mutate(date = floor_date(DateTime, "day")) %>%
  group_by(date, Site) %>% summarize(mean_temp_c = mean(`Temperature (C)`), mean_temp_f = mean(`Temperature (F)`))
view(avg.hobo.2022)

avg.hobo.2022 %>%
  ggplot(aes(x = date, y = mean_temp_f)) +
  geom_line() + 
  geom_smooth(method = "loess")  +
  xlab("Date") + ylab("mean temperature (F)") +
  ggtitle("")


avg.hobo.2022 %>% 
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_line(aes(color = Site)) + 
  xlab("Date") + ylab("mean temperature (C)") +
  ggtitle("mean temperature (C) 2022")

#adding add averages together#
avg.hobo.2022 <- na.omit(avg.hobo.2022)

avg.hobo <- rbind(avg.hobo.2019, avg.hobo.2020, avg.hobo.2021, avg.hobo.2022)

avg.hobo %>%
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_point(aes(color = Site), alpha = 0.5) + ggtitle("mean temperatue (C) all data") +
  ylab("mean temperature (C)")

avg.hobo %>%
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_point(alpha = 0.5) + ggtitle("mean temperatue (C) all data") +
  ylab("mean temperature (C)")

avg.hobo <- avg.hobo %>% mutate(Year = year(date))
avg.hobo %>% filter(Year == 2022) %>% filter(Site == "ST")
avg.hobo.2020 %>% group_by(Site) %>% summarise(max_f = max(mean_temp_f), max_c = max(mean_temp_c))

max.hobo <- avg.hobo %>% group_by (Year, Site) %>% summarize(max_f = max(mean_temp_f), max_c = max(mean_temp_c))

avg.hobo %>%
  ggplot(aes(x = date, y = mean_temp_c)) +
  geom_point(aes(color = Site), alpha = 0.5) +facet_wrap(~Year, ncol =1)
max.hobo
ST.2020 <- avg.hobo %>% filter(Site == "ST") %>% filter(Year == "2020")
ST.2020
ST.2020[which.max(ST.2020$mean_temp_c),]
HI.2020 <- avg.hobo %>% filter(Site == "HI") %>% filter(Year == "2020")
HI.2020
HI.2020[which.max(HI.2020$mean_temp_c),]
NH.2020 <- avg.hobo %>% filter(Site == "NH") %>% filter(Year == "2020")
NH.2020
NH.2020[which.max(NH.2020$mean_temp_c),]

ST.2021 <- avg.hobo %>% filter(Site == "ST") %>% filter(Year == "2021")
ST.2021
ST.2021[which.max(ST.2021$mean_temp_c),]
HI.2021 <- avg.hobo %>% filter(Site == "HI") %>% filter(Year == "2021")
HI.2021
HI.2021[which.max(HI.2021$mean_temp_c),]
NH.2021 <- avg.hobo %>% filter(Site == "NH") %>% filter(Year == "2021")
NH.2021
NH.2021[which.max(NH.2021$mean_temp_c),]

ST.2022 <- avg.hobo %>% filter(Site == "ST") %>% filter(Year == "2022")
ST.2022
ST.2022[which.max(ST.2022$mean_temp_c),]
HI.2022 <- avg.hobo %>% filter(Site == "HI") %>% filter(Year == "2022")
HI.2022
HI.2022[which.max(HI.2022$mean_temp_c),]

NH.2022 <- avg.hobo %>% filter(Site == "NH") %>% filter(Year == "2022")
NH.2022
NH.2022[which.max(NH.2022$mean_temp_c),]

vec <- c("2019-09-09", "2020-08-15", "2020-08-20", "2020-08-12", "2021-08-31", "2021-09-02", "2021-09-25", "2022-08-30", "2022-09-01", "2022-09-14")
vec
max.hobo$date <- vec
max.hobo
write_csv(max.hobo, "~/Desktop/WD/GSI/hobo.max.csv")


theme_set(theme_tufte()) 
theme_update(plot.title = element_text(hjust=0.5), #center plot titles
             axis.line.x = element_line(color="black", size=1), #add axis line axis.line.y = element_line(color="black", size=1),
             text = element_text(size=20), #change axis label sizes
             axis.text = element_text(size=15)) #center all titles and axis lines

