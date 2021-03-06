#Store the data after unzipping
activityData <- read.csv(file="activity.csv",header=TRUE)

#Calculate the total steps taken per day

totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

#Making the histogram

hist(totalSteps$steps, main= "Total Steps per Day",xlab = "Number of Steps")

#Calculate and report the mean

meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all days (y-axis)

library(ggplot2)
meanStepsByInterval <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInterval, aes(x=interval, y=steps))+
  geom_line()+
  ggtitle("Average Daily Activity")+
  xlab("5-min Interval")+
  ylab("Average number of steps")+
  theme(plot.title = element_text(hjust = 0.5))

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxIntervals <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]

#Calculate and report the total number of missing values in the dataset

missingVals <- is.na(activityData$steps)

#Devise a strategy for filling in all of the missing values in the dataset

imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInterval$steps[match(activityData$interval,
                                                                             meanStepsByInterval$interval)],
                                             activityData$steps))

#Make a histogram of the total number of steps taken each day and report the mean and median.

impStepsByInterval <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInterval$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")

impMeanSteps <- mean(impStepsByInterval$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInterval$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsByInterval$steps) - sum(totalSteps$steps)

# Create a new factor variable in the dataset with two levels - "weekend" and "weekday"


  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")

imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)

# Make a panel plot containnig a time-series plot of the 5-minute interval
# and the average number of steps taken across all weekdays or weekends

library(ggplot2)
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))

