# Calling all necessary libraries
library(dplyr)
library(tidyr)
library(plyr)
library(splitstackshape)
library(car)
library(lubridate)
library(rpart)

# Working directory and file import
train<-read.csv("Shelter.csv", header = T, stringsAsFactors = F)
head(train)
names(train)[1]<-'ID'
test<-read.csv("test.csv", header =T, stringsAsFactors = F)
test$ID<-as.character(test$ID)
data1<-bind_rows(train,test)

# Separating gender and sexual status
data1$SexuponOutcome<-ifelse(nchar(data1$SexuponOutcome)==0, 'Spayed Female', data1$SexuponOutcome)
data1$SexuponOutcome<-ifelse(data1$SexuponOutcome=="Unknown", 'Spayed Female', data1$SexuponOutcome)

data1<-cSplit(data1, "SexuponOutcome", sep=" ")
data1<-rename(data1, c("SexuponOutcome_1"="Sex_status", "SexuponOutcome_2"="Gender"))

# Create dummy variable - Is_active?
data1$Is_active<-ifelse(data1$Sex_status=="Intact",1,0)
table(data1$Is_active)

# Create dummmy variable - has_name
data1$has_name<-ifelse(data1$Name=="",0,1)
table(data1$has_name)

# Creating a variable to express the age
table(data1$AgeuponOutcome)
data1<-cSplit(data1, "AgeuponOutcome", sep = " ")
tail(data1)
data1<-rename(data1, c("AgeuponOutcome_1" = "Value", "AgeuponOutcome_2" = "unit"))
data1$unit<-gsub('s', '', data1$unit)
data1$Value<-as.numeric(data1$Value)
data1$unit<-as.factor(data1$unit)

multiplier<-ifelse(data1$unit == 'day', 1, 
                   ifelse(data1$unit == 'week', 7,
                          ifelse(data1$unit == 'month', 30,
                                 ifelse(data1$unit == 'year', 365, NA))))
data1$age_days <- multiplier * data1$Value
summary(data1$age_days)

# Creating a variable to indicate mixed breed
data1$Is_mix<-ifelse(grepl('Mix', data1$Breed),1,0)
some(data1)

# Create a variable for black color
data1$Is_black<-ifelse(grepl('black', data1$Color, ignore.case = TRUE),1,0)

# Time of day
data1$Hour<-hour(as.POSIXlt(data1$DateTime))
data1$Weekday<-wday(as.POSIXlt(data1$DateTime))
data1$Month<-month(as.POSIXlt(data1$DateTime))
data1$Year<-year(as.POSIXlt(data1$DateTime))

data1$TimeofDay <- ifelse(data1$Hour > 5 & data1$Hour < 11, 'morning',
                        ifelse(data1$Hour > 10 & data1$Hour < 16, 'midday',
                               ifelse(data1$Hour > 15 & data1$Hour < 20, 'evening', 'night')))

data1$TimeofDay <- factor(data1$TimeofDay, 
                         levels = c('morning', 'midday',
                                    'evening', 'night'))

# Imputation for age

age_fit <- rpart(age_days ~ AnimalType + Gender + Is_active + has_name + Is_mix, 
                 data = data1[!is.na(data1$age_days), ], 
                 method = 'anova')

data1$age_days[is.na(data1$age_days)] <- predict(age_fit, data1[is.na(data1$age_days), ])

# Create dummy for stage of development

data1$stage[data1$age_days < 365] <- 'baby'
data1$stage[data1$age_days >= 365 & data1$age_days<=3650 ] <- 'adult'
data1$stage[data1$age_days > 3650] <- 'old'
data1$stage<-as.factor(data1$stage)



