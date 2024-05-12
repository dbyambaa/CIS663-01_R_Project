#CIS663 "Individual Project in R" 
#Relationship between Tornadoes and Thunderstorm Wind
# Davaasuren Byambaa


# Set working Directory 
dirc <- "C:/Users/davab/OneDrive/Desktop/CIS663 Spring2024"
setwd(dirc)

# Downloading data from https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=21,KENTUCKY

# Before downloading the data set one should specify the state, date, and event.
# Then click on search and download csv file named "storm_data_search_results" save it in your working directory.
# The source only allows 500 records per data set therefor I have downloaded separate tables for records from "01/01/2003 -12/31/2012"
# and records from "01/01/2003-12/31/2023".

#Load Data into R
StormData<- read.csv("StormEvents_details-ftp_v1.0_d2023_c20240418.csv", header=TRUE)
head(StormData)

#Column names and descriptions used
#episode_id Ex: 61280, 62777, 63250-ID assigned by NWS to denote the storm episode; Episodes may contain multiple Events.The occurrence of storms and other significant weather phenomena having sufficient intensity to cause loss of life, injuries, significant property damage, and/or disruption to commerce.
#event_type Ex: Hail, Thunderstorm Wind, Snow, Ice (spelled out; not abbreviated)-The only events permitted in Storm Data are listed in Table 1 of Section 2.1.1 of NWS Directive 10-1605 at http://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf. The chosen event name should be the one that most accurately describes the meteorological event leading to fatalities, injuries, damage, etc. However, significant events, such as tornadoes, having no impact or causing no damage, should also be included in Storm Data
#magnitude Ex: 0.75, 60, 0.88, 2.75-The measured extent of the magnitude type ~ only used for wind speeds (in knots) and hail size(in inches to the hundredth).
#tor_f_scale Ex: EF0, EF1, EF2, EF3, EF4, EF5
#Enhanced Fujita Scale describes the strength of the tornado based on the amount and type of
#damage caused by the tornado. The F-scale of damage will vary in the destruction area;
#therefore, the highest value of the F-scale is recorded for each event.
##EF0 – Light Damage (40 – 72 mph)
##EF1 – Moderate Damage (73 – 112 mph)
##EF2 – Significant damage (113 – 157 mph)
##EF3 – Severe Damage (158 – 206 mph)
##EF4 – Devastating Damage (207 – 260 mph)
##EF5 – Incredible Damage (261 – 318 mph)
##EFU - tornadoes with lack of damage report


#Cleaning and organizing the dataset 
install.packages("dplyr")
install.packages("ggplot")
require(dplyr)
library(ggplot2)

#select columns from the downloaded data 
Tornadoes<- StormData%>%select(EPISODE_ID, EVENT_TYPE, MAGNITUDE, TOR_F_SCALE)
colnames(Tornadoes)

#Creating new table for analyze  
#First only draw out event types will be used in the analyze
df_raw<-Tornadoes[Tornadoes$EVENT_TYPE == c("Tornado", "Thunderstorm Wind"),]


#Finding average wind speed for the weather episode and removing Na and NAN
Thunderstorm<-df_raw %>% group_by(EPISODE_ID) %>% summarize(ThunderstormWind=mean(MAGNITUDE, na.rm=TRUE))
Thunderstorm<-(na.omit(Thunderstorm))

#Merging the the column back to my data frame
final<-merge(df_raw, Thunderstorm, by="EPISODE_ID")

#Excluding the columns that will not be used in analyze
final<-final[-c(2,3)]

#Excluding the duplicates 
final<-final%>%distinct()

#Looking at the unique value in Tornado and replacing them with numbers
unique(final$TOR_F_SCALE)
final$TOR_F_SCALE[final$TOR_F_SCALE == ""]<-1
final$TOR_F_SCALE[final$TOR_F_SCALE == "EFU"]<-2
final$TOR_F_SCALE[final$TOR_F_SCALE == "EF0"]<-3
final$TOR_F_SCALE[final$TOR_F_SCALE == "EF1"]<-4
final$TOR_F_SCALE[final$TOR_F_SCALE == "EF2"]<-5
final$TOR_F_SCALE[final$TOR_F_SCALE == "EF3"]<-6
final$TOR_F_SCALE[final$TOR_F_SCALE == "EF4"]<-7

#PLot
ggplot(final, aes(x=ThunderstormWind, y=TOR_F_SCALE)) + geom_point()+geom_smooth(method="lm")
  labs(x="ThunderstormWind", y="TOR_F_SCALE") 

wind.tornado<-final%>%select(ThunderstormWind, TOR_F_SCALE)
plot(wind.tornado)

#Replacing wind speeds with smaller numbers to fix the issue of negative intercept. 
unique(final$ThunderstormWind)
final$ThunderstormWind[final$ThunderstormWind <= "10"]<-1
final$ThunderstormWind[final$ThunderstormWind > "10" & final$Thunderstorm <="20"]<-2
final$ThunderstormWind[final$ThunderstormWind > "20" & final$Thunderstorm <="30"]<-3
final$ThunderstormWind[final$ThunderstormWind > "30" & final$Thunderstorm <="40"]<-4
final$ThunderstormWind[final$ThunderstormWind > "40" & final$Thunderstorm <="50"]<-5
final$ThunderstormWind[final$ThunderstormWind > "50" & final$Thunderstorm <="60"]<-6
final$ThunderstormWind[final$ThunderstormWind > "60" & final$Thunderstorm <="70"]<-7
final$ThunderstormWind[final$ThunderstormWind > "70" & final$Thunderstorm <="80"]<-8
final$ThunderstormWind[final$ThunderstormWind > "80" & final$Thunderstorm <="90"]<-9
final$ThunderstormWind[final$ThunderstormWind > "90" & final$Thunderstorm <="100"]<-10

#Linear Regression
final_lm<-lm(TOR_F_SCALE~ThunderstormWind, data=final)
summary(final_lm)









