library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
colorsData <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#reading data
apr_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-apr14.csv")
may_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-may14.csv")
jun_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-jun14.csv")
jul_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-jul14.csv")
aug_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-aug14.csv")
sep_data <- read.csv("C:\\Users\\KIIT\\Downloads\\uber-raw-data-sep14.csv")

#combining all files in one dataset
uberData <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

dim(uberData)

#formatting of data and time column
uberData$Date.Time <- as.POSIXct(uberData$Date.Time,format="%m/%d/%Y%H:%M:%S")
uberData$Time <- format(as.POSIXct(uberData$Date.Time,format="%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uberData$Date.Time <- ymd_hms(uberData$Date.Time)
#factors of time objects
uberData$day <- factor(day(uberData$Date.Time))
uberData$month <- factor(month(uberData$Date.Time,label = TRUE))
uberData$year <- factor(year(uberData$Date.Time))
uberData$daysofweek <- factor(wday(uberData$Date.Time,label = TRUE))
uberData$hour <- factor(hour(hms(uberData$Time)))
uberData$minute <- factor(minute(hms(uberData$Time)))
uberData$second <- factor(second(hms(uberData$Time)))

#plotting the trips by hours in a day
dataUber <- uberData
hourData <- dataUber%>%group_by(hour)%>%
  dplyr::summarize(Total=n())
head(hourData,10)

#trips every hour
ggplot(hourData,aes(hour,Total))+
  geom_bar(stat = "identity",fill="orange",color="black")+
  ggtitle("Trips Every Hour")+
  theme(legend.position = "none")+theme_light()+
  scale_y_continuous(labels = comma)+xlab("Hour")+ylab("Total Trips")

#trips by hour and month
monthHour <- dataUber%>%group_by(month,hour)%>%
  dplyr::summarize(Total=n())
ggplot(monthHour,aes(hour,Total,fill=month))+
  geom_bar(stat = "identity")+ggtitle("Trips By Hour And Month")+
  theme_light()+scale_y_continuous(labels = comma)+xlab("Hour")

#plotting no of trips taking place during months in a year
monthGroup <- dataUber%>%
  group_by(month)%>%
  dplyr::summarize(Total=n())
head(monthGroup)
ggplot(monthGroup,aes(month,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Month")+theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+theme_light()+xlab("Month")

#plotting trips by day and month
daymg <- dataUber%>%
  group_by(month,day,daysofweek)%>%
  dplyr::summarize(Total=n())

ggplot(daymg,aes(month,Total,fill=daysofweek))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Trips By Day And Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colorsData)+xlab("Month")+
  theme_light()

#trips by bases
ggplot(dataUber,aes(Base))+
  geom_bar(fill="salmon")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips By Bases")+ylab("Count")+
  theme_light()

#trips by bases and months
ggplot(dataUber,aes(Base,fill=month))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips By Bases And Month")+
  scale_fill_manual(values = colorsData)+ylab("Count")+
  theme_light()