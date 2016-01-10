##Using R version 3.2.2 (2015-08-14) -- "Fire Safety"

##set working directory to include Activity Monitoring Data

library(dplyr)
library(ggplot2)

# Read in activity data
activity_data <- read.csv("activity.csv")

## What is the mean total number of steps taken per day?

     # Question 1: Calculate the total number of steps taken per day.
          by_date<- group_by(activity_data,date)
          steps_per_day<-summarise(by_date,sum(steps,na.rm=TRUE))
          names(steps_per_day)[2]<-"steps"

     # Question 2: Make a histogram of the total number of steps taken each day.
          hist(steps_per_day$steps,main="Total number of steps taken each day",
                    xlab = "Number of Steps",col = "gray")

     # Question 3: Calculate and report the mean and median of the total number
     # of steps taken per day
          mean_steps_per_day <- mean(steps_per_day$steps)
          median_steps_per_day<-median(steps_per_day$steps)

## What is the average daily activity pattern?

     # Question 1: Make a time series plot (i.e. type = "l") of the 5-minute
     # interval (x-axis) and the average number of steps taken, averaged across
     # all days (y-axis)
          by_interval<-group_by(activity_data,interval)
          mean_by_interval<-summarise(by_interval,mean(steps,na.rm=TRUE))
          names(mean_by_interval)[2]<-"average steps"
          plot(mean_by_interval$interval,mean_by_interval$`average steps`,
               type="l",main="Average number of steps taken by interval",
               xlab="Interval",ylab="Number of steps")

     # Question 2: Which 5-minute interval, on average across all the days in 
     # the dataset, contains the maximum number of steps?
          filter(mean_by_interval,mean_by_interval$`average steps`==
                      max(mean_by_interval$`average steps`))

## Imputing missing values

     # Question 1: Calculate and report the total number of missing values in
     # the dataset (i.e. the total number of rows with NAs)
          number_of_missing_values<-sum(is.na(activity_data))
          number_of_missing_values

     # Question 2: Devise a strategy for filling in all of the missing values in
     # the dataset. The strategy does not need to be sophisticated. For example,
     # you could use the mean/median for that day, or the mean for that 5-minute
     # interval, etc.
          # I have decided to use the mean for the 5-minute interval to fill in
          # the missing values.  This is because it appears missing values are
          # for entire days, so using the daily mean would result in all 0's.

     # Question 3: Create a new dataset that is equal to the original dataset
     # but with the missing data filled in.
          new<-activity_data %>%
          group_by(interval) %>%
          mutate(steps=ifelse(is.na(steps), mean(steps, na.rm=TRUE),steps))

     # Question 4: Make a histogram of the total number of steps taken each day
     # and Calculate and report the mean and median total number of steps taken
     # per day
          new_steps_per_day<-new %>%
               group_by(date) %>%
               summarise(sum(steps))
          names(new_steps_per_day)[2]<-"steps"
          hist(new_steps_per_day$steps,
               main="Total number of steps taken each day",
               xlab = "Number of Steps",col = "gray")

          new_mean_steps_per_day <- mean(new_steps_per_day$steps)
          new_median_steps_per_day<-median(new_steps_per_day$steps)
          new_mean_steps_per_day
          new_median_steps_per_day

## Are there differences in activity patterns between weekdays and weekends?

     # Question 1: Create a new factor variable in the dataset with two levels
     # "weekday" and "weekend" indicating whether a given date is a weekday or
     # weekend day.
          weekend_days<-c("Saturday","Sunday")
          new$date<-as.Date(new$date)
          new$day<-factor((weekdays(new$date) %in% weekend_days), 
                          levels=c(FALSE,TRUE),labels=c('weekday','weekend'))

     # Question 2: Make a panel plot containing a time series plot (i.e.
     # type = "l") of the 5-minute interval (x-axis) and the average number of
     # steps taken, averaged across all weekday days or weekend days (y-axis)
          new_by_interval<-group_by(new,day,interval) %>%
               summarise(mean(steps))
          names(new_by_interval)[3]<-"average steps"

          panel_plot<-ggplot(new_by_interval,aes(interval,`average steps`)) + 
               geom_line()
          panel_plot + facet_grid(day ~.)
