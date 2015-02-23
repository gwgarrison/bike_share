library(randomForest);library(caret)
library(ggplot2);library(dplyr);library(lubridate);library(ggmap);library(GGally)
library(gridExtra)

training <- tbl_df(read.csv("train.csv"))
testing <- tbl_df(read.csv("test.csv"))

# modify and add variables
training$season <- factor(training$season)
training$workingday <- factor(training$workingday)
training$holiday <- factor(training$holiday )
training$hr <- factor(hour(training$datetime))
training$wdy <- factor(wday(training$datetime,label = TRUE))
training$mth <- factor(month(training$datetime,label = TRUE))
training$weather <- factor(training$weather)
training <- select(training,-casual,-registered)

testing$season <- factor(testing$season)
testing$workingday <- factor(testing$workingday)
testing$holiday <- factor(testing$holiday )
testing$hr <- factor(hour(testing$datetime))
testing$wdy <- factor(wday(testing$datetime,label = TRUE))
testing$mth <- factor(month(testing$datetime,label = TRUE))
testing$weather <- factor(testing$weather)

# create daypart
training$daypart <- 4
testing$daypart <- 4

training$hr <- as.numeric(training$hr)
testing$hr <- as.numeric(testing$hr)

#4AM - 10AM = 1
training$daypart[(training$hr < 10) & (training$hr > 3)] <- 1
testing$daypart[(testing$hr < 10) & (testing$hr > 3)] <- 1

#11AM - 3PM = 2
training$daypart[(training$hr < 16) & (training$hr > 9)] <- 2
testing$daypart[(testing$hr < 16) & (testing$hr > 9)] <- 2

#4PM - 9PM = 3
training$daypart[(training$hr < 22) & (training$hr > 15)] <- 3
testing$daypart[(testing$hr < 22) & (testing$hr > 15)] <- 3

training$daypart <- as.factor(training$daypart)
testing$daypart <- as.factor(testing$daypart)

training$hr <- factor(training$hr)
testing$hr <- factor(testing$hr)

# create sunday variable
#create Sunday variable
training$sunday[training$wdy == "Sun"] <- "1"
training$sunday[training$wdy != "Sun"] <- "0"

testing$sunday[testing$wdy == "Sun"] <- "1"
testing$sunday[testing$wdy != "Sun"] <- "0"

#convert to factor
training$sunday <- as.factor(training$sunday)
testing$sunday <- as.factor(testing$sunday)
