
library(stargazer)
library(rdd)
library(StatMeasures)
library(rddensity)
library(tidyverse)
library(highlight)
library(easycsv)
library(data.table)
library(fread)
library(Rmisc)
library(stringr)
library(haven)
library(foreign)
library(readr)
library(effsize)
library(ggplot2)
library(vctrs)
library(ggpubr)
library(plyr)
library(dplyr)
library(Rmisc)
library(reshape)
library(ggthemes)
library(gridExtra)
library(car)
library(corrplot)

#This is the cleaning script for the Men's 1600 Meters Data#

##########################    DATA CLEANING STARTS HERE   #####################

#Import aggregate data & Clean

master <- read.csv("C:/Users/rowse/Dropbox/Track_Times/Data/Mens Master Data/mastertrack_data.csv")

#Create 1600 Meter Dataset
data1600meters <- subset(master, event == "1600 Meters")

#Get rid of "X" column

data1600meters$X <- NULL

#Number of Athletes
length(unique(data1600meters$AID))

#Create Handtime Dummy Column
data1600meters$handtime <- NA
data1600meters$lastcharacter <- sapply(strsplit(as.character(data1600meters$time), ""), tail, 1)
data1600meters$handtime <- ifelse(data1600meters$lastcharacter == "h", 1, 0)

#Go through table and figure out what each last character means
table(data1600meters$lastcharacter)

#What is F, R, S, & T

Fdata <- subset(data1600meters, lastcharacter == "F")
#F is DNF == "Did Not Finish"

Rdata <- subset(data1600meters, lastcharacter == "R")
#R is SCR == "Scratched from race"

Sdata <- subset(data1600meters, lastcharacter == "S")
#R is DNS == "Did not start"

Tdata <- subset(data1600meters, lastcharacter == "T")
#T is NT == "Not Timed"

Fdata <- NULL
Rdata <- NULL
Sdata <- NULL
Tdata <- NULL

#Now that we have the dummy column, we can remove "h"
look_for <- "[h]$"
replace_with <- ""

data1600meters$time <- str_replace(data1600meters$time, look_for, replace_with)

data1600meters$lastcharacter <- NULL

#DNF, DNS, DQ, NT, SCR all need to be removed from the data
data1600meters <- data1600meters[!(data1600meters$time == "DNF"),]
data1600meters <- data1600meters[!(data1600meters$time == "DNS"),]
data1600meters <- data1600meters[!(data1600meters$time == "DQ"),]
data1600meters <- data1600meters[!(data1600meters$time == "SCR"),]
data1600meters <- data1600meters[!(data1600meters$time == "NT"),]

#Dummy Outdoor Events
data1600meters$outdoor <- NA
data1600meters$outdoor <- ifelse(grepl("Outdoor", data1600meters$year.season), 1, 0)

#Create a Minute Column

data1600meters$minute <- NA
data1600meters$minute <- sub(":.*", "", data1600meters$time)
data1600meters$minute <- as.integer(data1600meters$minute)

#Convert Minute to Seconds

data1600meters$minbysec <- NA
data1600meters$minbysec <- data1600meters$minute * 60

data1600meters$minute <- NULL

#Create a Seconds Column
data1600meters$seconds <- NA
data1600meters$seconds <- sub(".*:", "", data1600meters$time)
data1600meters$seconds<- as.numeric(data1600meters$seconds)

class(data1600meters$seconds)

#Correct Time (adding minbysec and seconds columns together)

data1600meters$time <- data1600meters$seconds + data1600meters$minbysec
data1600meters$time <- as.numeric(data1600meters$time)


#Create Integer Time
data1600meters$inttime <- as.integer(data1600meters$time)

time_histogram <- ggplot(data1600meters, aes(x = time)) + 
  geom_histogram(binwidth = 1) + xlim(225, 500)
time_histogram

#Let's address the outliers below
#Set the minimum at 3 min 53 seconds because that is the US High School National Record
#Set the maximum 

data1600meters <- subset(data1600meters, time > 232 & time < 425)

time_histogram <- ggplot(data1600meters, aes(x = time)) + 
  geom_histogram(binwidth = 1) + xlim(225, 500)
time_histogram

#Get rid of redundant time data
data1600meters$minute <- NULL
data1600meters$seconds <- NULL
data1600meters$minbysec <- NULL


#Fix the Grade

data1600meters$grade <- sub("th Grade.*", "", data1600meters$grade)
table(data1600meters$grade)
typeof(data1600meters$grade)
data1600meters$grade <- as.numeric(data1600meters$grade)

#Keep Only High School Data
data1600meters <- subset(data1600meters, data1600meters$grade > 8)
table(data1600meters$grade)

#Make Grade a Character to Create id in Stata
data1600meters$grade <- as.character(data1600meters$grade)



#Make AID a Character Column to Avoid Exponential Tranformations in STATA
data1600meters$AID <- as.character(data1600meters$AID)


write.dta(data1600meters, 'C:/Users/rowse/Dropbox/Track_Times/Data/Mens_1600_Data/new_clean1600data.dta')
