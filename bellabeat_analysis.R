#Loading the needed packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

#Data imported by using RStudio from local computer

activity <- read.csv("tracker_data/dailyActivity_merged.csv")
calories <- read.csv("tracker_data/dailyCalories_merged.csv")
sleep <- read.csv("tracker_data/sleepDay_merged.csv")
weight <- read.csv("tracker_data/weightLogInfo_merged.csv")
intensities <- read.csv("tracker_data/dailyIntensities_merged.csv")
heartrate <- read.csv("tracker_data/heartrate_seconds_merged.csv")

#Check that importing was done correctly
head(sleep)
View(weight)
View(activity)
View(calories)
View(intensities)
View(heartrate)

#Date format needs to be converted to date type


activity$ActivityDate <- as.Date(activity$ActivityDate, "%m/%d/%Y")
weight$Date <- as.Date(weight$Date, "%m/%d/%Y")
sleep$SleepDay <- as.Date(sleep$SleepDay, "%m/%d/%Y")
calories$ActivityDay <- as.Date(calories$ActivityDay, "%m/%d/%Y")
intensities$ActivityDay <- as.Date(intensities$ActivityDay, "%m/%d/%Y")

#Naming of the date column will be unified
activity <- activity %>% 
  rename(Date = ActivityDate)
sleep <- sleep %>% 
  rename(Date = SleepDay)
intensities <- intensities %>% 
  rename(Date = ActivityDay)
calories <- calories %>% 
  rename(Date = ActivityDay)


#Next it is time to check if there are any missing values (NA).

sum(is.na(activity)) #0
sum(is.na(weight)) #65
sum(is.na(sleep)) #0
sum(is.na(intensities)) #0
sum(is.na(calories)) #0
sum(is.na(heartrate)) #0

#By checking na, we can see that weight dataframe has 65 NAs.
#Let's examine it closer

(is.na(weight))

#The NA values resists in fat column. Since there are so many
#missing values, this column can't be used for analysis.

#Let's check how many unique users have provided to every dataset
#Note that there was 33 persons who answered the survey

n_distinct(activity$Id) #33
n_distinct(weight$Id) #8
n_distinct(sleep$Id) #24
n_distinct(intensities$Id) #33
n_distinct(calories$Id) #33
n_distinct(heartrate$Id) #14

#Activity, intensities and calories have full response rate. (Activity has partly same data as intensity)
#Sleep has 24 respondent which can be considered to be okay
#Problems arise with heartrate and weight dataframes.

#Let's check the summaries for each datasets. We will focus on the three datasets
#with highest response rate. 
summary(activity)
summary(intensities)
summary(calories)
summary(sleep)

#Let's convert min to hours for readability.
sleep<- sleep %>% 
  mutate(total_hours_asleep= TotalMinutesAsleep/60) %>% 
  mutate(total_hours_in_bed = TotalTimeInBed/60)

head(sleep)

#Let's now aggregate the dataframes to find averages per user.

#sleep

sleep_sum <- aggregate(cbind(total_hours_asleep,total_hours_in_bed)~Id,
                           sleep,mean)
colnames(sleep_sum)[2:3] <- c('average_asleep_hours','average_in_bed_hours')
sleep_sum[,-1] <- round(sleep_sum[,-1],0)

View(sleep_sum)

#activity

activity_sum <- aggregate(cbind(VeryActiveMinutes, FairlyActiveMinutes, 
                                    LightlyActiveMinutes, SedentaryMinutes)~Id,activity,mean)
colnames(activity_sum)[2:5] <- c('average_very_active_min', 
                                     'average_fairly_active_min',
                                     'average_lightly_active_min', 
                                     'average_sedentary_min')

activity_sum[,-1] <-round(activity_sum[,-1],0)
head(activity_sum)

#calories

calories_average <- aggregate(cbind(Calories)~Id,
                              calories,mean)
colnames(calories_average) [2:3] <- c('average_calories')
calories_average[,-1] <- round(calories_average[,-1],0)
head(calories_average)
View(calories_average)

#Merging the summary dataframes
merged_data <- merge(sleep_sum, activity_sum,by="Id")
merged <- merge(merged_data,calories_average,by="Id")
View(merged)

#To be able to compare daily activity versus sedentary(inactive),let's merge
#columns measuring activity

merged <- merged %>% 
  mutate(average_daily_activity= average_very_active_min + average_fairly_active_min + average_lightly_active_min)

#Let's now change this and sedentary_acitivity min to hours
merged<- merged %>% 
  mutate(average_daily_activity_hours= average_daily_activity/60) %>% 
  mutate(average_sedentary_hours = average_sedentary_min/60)


#Transfering the data to csv for data viz in Tableau & Sheets.
write.csv(merged,"C:\\Users\\selja\\Documents\\R\\capstone\\bellabeat.csv", row.names = FALSE)

