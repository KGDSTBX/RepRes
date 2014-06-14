

setwd("C:/Data/Analytics/Courses/Coursera/Reproducible Research/Assignment")
library(knitr)
library(lattice)
library(ggplot2)
library(mice)
library(date)

# Loading and preprocessing the data
# Show any code that is needed to
# 1.Load the data (i.e. read.csv())
# 2.Process/transform the data (if necessary) into a format suitable for your analysis


activity=read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
str(activity)
summary(activity)

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1.Make a histogram of the total number of steps taken each day
# 2.Calculate and report the mean and median total number of steps taken per day


totalsteps=tapply(activity$steps,activity$date,sum)
hist(totalsteps)
mean(totalsteps,na.rm=T)
median(totalsteps,na.rm=T)

# What is the average daily activity pattern?
# 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

intervalsteps=tapply(activity$steps,activity$interval,sum,na.rm=T)
hist(intervalsteps)
which(intervalsteps == max(intervalsteps),arr.ind = TRUE)

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

apply(activity, 2, function(x) length(which(is.na(x))))
set.seed(144)
activityimp=activity

activityimp = complete(mice(activity))
apply(activity, 2, function(x) length(which(is.na(x))))
totalstepsimp=tapply(activityimp$steps,activityimp$date,sum)
hist(totalstepsimp)
mean(totalstepsimp,na.rm=T)
median(totalstepsimp,na.rm=T)

#Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
#1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

activityimp$date=as.ate(activityimp$date)
weekdays=weekdays(as.date(activityimp$date))
activityimp$weekday=ifelse(activityimp$weekdays=="Saturday" | activityimp$weekdays=="Sunday", "Weekends","Weekdays")
tapply(activityimp$steps,activityimp$weekday,mean)