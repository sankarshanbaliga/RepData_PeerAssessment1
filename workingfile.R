setwd("E:/Work/coursera/Reproducible Research/RepData_PeerAssessment1")

#reading csv files
activity <- read.csv("activity.csv")

#processing data
library(reshape2)
activity_melt <- melt(activity, id = c("date", "interval"))
activity_data <- dcast(activity_melt, interval ~ date)


# Mean Total number of steps per day
daily_steps <- colSums(activity_data[,2:62])
hist(daily_steps)
mean.daily.steps <- mean(daily_steps, na.rm = TRUE)
median.daily.steps <- median(daily_steps, na.rm = TRUE)

#Average daily activity pattern
activity_data$intervalmeans <- rowMeans(activity_data[,2:62], na.rm = TRUE)
plot(activity_data$intervalmeans, type = "l")
activity_data$maxactivity <- activity_data$intervalmeans == 
    max(activity_data$intervalmeans, na.rm= TRUE)

#interval with max activity
max_interval <- activity_data[activity_data$maxactivity == 1,]$interval

#Number of missing values
missingvals <- sum(is.na(activity_data[-c(1,63,64)]))

#imputing missing values - using rowmeans
for(i in 1:nrow(activity_data)){
    activity_data[i, is.na(activity_data[i,])] <- 
        round(activity_data$intervalmeans[i],0)
}

# check if any missing values
missingvals2 <- sum(is.na(activity_data[-c(1,63,64)]))

# After removing NA -  Mean Total number of steps per day
daily_steps_new <- colSums(activity_data[,2:62])
hist(daily_steps_new)
new.mean.dailysteps <- mean(daily_steps_new)
new.median.dailysteps <- median(daily_steps_new)

#impact of imputations
change.mean <- mean.daily.steps - new.mean.dailysteps
change.median <- median.daily.steps - new.median.dailysteps

activity_data <- activity_data[-c(289:290),]

#transposing dataframe
activity_data_transpose <- as.data.frame(t(activity_data))

#Getting date and time values in
activity_data_transpose$newdates <- rep(NA,64)
datetime <- rownames(activity_data_transpose)
datetime[c(1,63,64)] <- ""
activity_data_transpose$newdates <- as.Date(datetime,"%Y-%m-%d")

#to find out if day is a weekday or weekend
library("timeDate")
activity_data_transpose$daytest <- NA
for(i in 2:62){
    if(isWeekday(activity_data_transpose$newdates[i])) activity_data_transpose$daytest[i] <- "Weekday"
    if(isWeekend(activity_data_transpose$newdates[i])) activity_data_transpose$daytest[i] <- "Weekend"
}
activity_data_transpose$daytest <- as.factor(activity_data_transpose$daytest)

#processing data for plotting
names(activity_data_transpose)[1:288] <- activity_data[,1]
activity_data_transpose <- activity_data_transpose[,c(1:288,290)]
names(activity_data_transpose)
activity_transpose_melt <- melt(activity_data_transpose, id = c("daytest"), na.rm = TRUE)
names(activity_transpose_melt) = c("daytest", "interval", "value")

## where activity melt has NA's in daytest column - remove
data_melt.sub <- subset(activity_transpose_melt, daytest != "")

#converting character values to numeric
data_melt.sub$value <- as.numeric(data_melt.sub$value)


# Actual Plotting
library("lattice")
xyplot(value~interval | daytest, data = data_melt.sub, type = "l",
       layout=c(1,2),xlab="Interval",ylab = "Number of steps")

# Average data on weekdays and weekends for every interval
plotsteps <- dcast(data_melt.sub, interval ~ daytest, mean)

