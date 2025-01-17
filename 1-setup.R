library(randomForest);library(caret)
library(ggplot2);library(dplyr);library(lubridate);library(ggmap);library(GGally)
library(gridExtra);library(party);library(Metrics)

training <- tbl_df(read.csv("train.csv"))
testing <- tbl_df(read.csv("test.csv"))

# modify and add variables
training$season <- factor(training$season)
training$workingday <- factor(training$workingday)
training$holiday <- factor(training$holiday )
training$hr <- factor(hour(training$datetime))
training$wdy <- factor(wday(training$datetime,label = TRUE))
training$mth <- factor(month(training$datetime,label = TRUE))
training$yr <- factor(year(training$datetime))
training$weather <- factor(training$weather)
training <- select(training,-casual,-registered)

testing$season <- factor(testing$season)
testing$workingday <- factor(testing$workingday)
testing$holiday <- factor(testing$holiday )
testing$hr <- factor(hour(testing$datetime))
testing$wdy <- factor(wday(testing$datetime,label = TRUE))
testing$mth <- factor(month(testing$datetime,label = TRUE))
testing$yr <- factor(year(testing$datetime))
testing$weather <- factor(testing$weather)

# create daypart
training$daypart <- 4
testing$daypart <- 4

training$hr <- as.numeric(training$hr)
testing$hr <- as.numeric(testing$hr)

# changed daypart to reflect busy and non busy times
#4AM - 10AM = 1
#8AM - 10AM = 1
training$daypart[(training$hr < 11) & (training$hr > 7)] <- 1
testing$daypart[(testing$hr < 11) & (testing$hr > 7)] <- 1

#11AM - 3PM = 2
#11AM - 4PM = 2
training$daypart[(training$hr < 17) & (training$hr > 10)] <- 2
testing$daypart[(testing$hr < 17) & (testing$hr > 10)] <- 2

#4PM - 9PM = 3
#17PM - 8PM = 3
training$daypart[(training$hr < 21) & (training$hr > 16)] <- 3
testing$daypart[(testing$hr < 21) & (testing$hr > 16)] <- 3

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

# create windy variable
training$windy[training$windspeed >= 40]<- "1"
training$windy[training$windspeed < 40] <- "0"

testing$windy[testing$windspeed >= 40]<- "1"
testing$windy[testing$windspeed < 40] <- "0"

training$windy <- as.factor(training$windy )
testing$windy <- as.factor(testing$windy )

# change datetime to date
training$datetime <- parse_date_time(as.character(training$datetime),"%Y%m%d %H%M%S")
testing$datetime <- parse_date_time(as.character(testing$datetime),"%Y%m%d %H%M%S")

# split data into training and validation sets
set.seed(77)
inTrain <- createDataPartition(y=training$count,p=0.6, list=FALSE)
training <- training[inTrain,]
validation<- training[-inTrain,]
