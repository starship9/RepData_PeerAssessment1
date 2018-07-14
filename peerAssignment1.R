activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
summary(activity)

mean(activity$steps, na.rm = TRUE)
median(activity$steps, na.rm = TRUE)
hist(activity$steps)

totalSteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
totalSteps
hist(totalSteps$steps)
mean(totalSteps$steps)
median(totalSteps$steps)

stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
stepsInterval
plot(steps~interval,data=stepsInterval,type="l")

max(stepsInterval$steps)
stepsInterval[which.max(stepsInterval$steps),]$interval

head(activity)
tail(activity)
summary(activity$steps)
dim(activity)


tapply(activity$steps,as.factor(activity$date),sum)
tapply(activity$steps,activity$date,mean)
tapply(activity$steps,activity$date,median)

#Time series plot
plot(activity$interval,activity$steps,type = "l")


summary(activity)
sum(is.na(activity))
colSums(is.na(activity))

#New Dataframe
activityMissingFilled <- activity

#Filling in missing values
activityMissingFilled$steps[is.na(activity$steps)] <- mean(activity$steps,na.rm = TRUE)

hist(activityMissingFilled$steps)
mean(activityMissingFilled$steps)
median(activityMissingFilled$steps)
weekdays(as.Date(activityMissingFilled$date),abbreviate = TRUE)

#activityMissingFilled$isWeekday <- weekdays(as.Date(activityMissingFilled$date))
#summary(activityMissingFilled)

factor(weekdays(as.Date(activityMissingFilled$date)),levels = c("Weekday","Weekend"))
head(weekdays(as.Date(activityMissingFilled$date)))
tail(weekdays(as.Date(activityMissingFilled$date)))

library(chron)
is.weekend(as.Date(activityMissingFilled$date))

library(timeDate)
tail(isWeekday(as.Date(activityMissingFilled$date), wday = 1:4))

activityMissingFilled$typeOfDay[isWeekday(as.Date(activityMissingFilled$date))] <- "weekday"
activityMissingFilled$typeOfDay[!isWeekday(as.Date(activityMissingFilled$date))] <- "weekend"
activityMissingFilled$typeOfDay <- as.factor(activityMissingFilled$typeOfDay)
table(activityMissingFilled$typeOfDay)

library(lattice)
xyplot(steps~interval | factor(typeOfDay), data = activityMissingFilled, aspect = 1/2, type = "l")

