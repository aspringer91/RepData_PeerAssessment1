---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

This is a submission for the week 2 course project of the Reprodicible Research course. The assignment does some quick analysis of activity data for a step counter in 5 minute intervals. The data set includes date, steps, and interval.


```r
# Read in the given data
data <- read.csv("activity.csv")
# Convert the date column to the Date class
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

This part of the assignment ignores the NA data. We are asked to calculate the total number of steps taken per day and create a histogram of the data.


```r
#remove NA data and aggregate
data_remove_na <- data[!is.na(data$steps),]
total_by_day <- aggregate(steps ~ date, data_remove_na, sum)
# create the histogram
hist(total_by_day$steps, main = "Histogram of steps per day", xlab = "steps", breaks = 10)
```

![](PA1_template_files/figure-html/total_steps_plot-1.png)<!-- -->

Next we will calculate and report the mean and median numbers of total steps taken per day.


```r
mean_steps <- as.integer(mean(total_by_day$steps))
median_steps <- as.integer(median(total_by_day$steps))
```

The mean number of steps per day is 10766 and the median number of steps per day is 10765

## What is the average daily activity pattern?

This part of the assignment investigates the average daily activity pattern by calculating the average number of steps taken in each 5 minute interval across all days. First, we will plot the average daily pattern.


```r
#aggregate the data by interval and plot
daily_mean <- aggregate(steps ~ interval, data, mean)
with(daily_mean, plot(interval, steps,type="l"))
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

Next, we will calculate and report the interval with the highest average step count.


```r
max_row <- daily_mean[which.max(daily_mean$steps),]
max_interval <- max_row$interval
```

The interval that has the highest number of steps on average is 835.

## Imputing missing values

We will now investigate the impact of missing data on the analysis and impute values where they are missing. My strategy for filling missing data will be to use the average of the 5-minute interval calculated in the previous section. First, calculate the total number of missing values.


```r
missing_values <- sum(!complete.cases(data))
```

There are 2304 in the dataset.
Next, fill the missing values using the average for that 5 minute interval across all days.


```r
# function to determine the average for a given 5 minute interval 
lookup_val <- function(x) {
    val <- daily_mean[daily_mean$interval == x, "steps"]
    val
}
# apply to all rows of the data a function that imputes the average if a value is missing
vect <- apply(data[,c('interval','steps')], 1, function(x) {if(is.na(x[2])) {lookup_val(x[1])} else {x[2]}})

data_impute <- data
data_impute$steps <- vect
```

Create a histagram of the data with the imputed values.


```r
total_by_day_impute <- aggregate(steps ~ date, data_impute, sum)
hist(total_by_day_impute$steps,  main = "Histogram of steps per day", xlab = "steps", breaks = 10)
```

![](PA1_template_files/figure-html/imputed hist-1.png)<!-- -->

Finally, calcualte and report the mean and median of the total daily steps with the imputed values.


```r
mean_steps <- as.integer(mean(total_by_day_impute$steps))
median_steps <- as.integer(median(total_by_day_impute$steps))
```

With the imputed data, the mean total number of steps taken per day is 10766 and the median total number of steps taken per day is 10766. Imputing the missing values did not have a significant impact on the mean or median measures. 


## Are there differences in activity patterns between weekdays and weekends?

In this section we will compare weekend activity to weekday activity. First, we will use the weekdays() function to determine the day of the week and assign either weekday or weekend to each observation. 


```r
# function that determines weekend or weekday given a day of the week
weekend_func <- function (day) {
    if (day == "Saturday" | day == "Sunday") {
        val <- "weekend"
    } else {
        val <- "weekday"
    }
    return(val)
}

# apply the weekend function to each row of the dataframe to assign weekend or weekday
weekday_vect <- sapply(data_impute$date, function(x) {weekend_func(weekdays(x))})
 
data_impute$weekday <- as.factor(weekday_vect)
```

Plot the data for weekdays and weekends.


```r
library(dplyr)
```


```r
library(ggplot2)
# use dplyr to group the data by weekday and average the steps
daily_mean <- data_impute %>%
              group_by(weekday,interval) %>%
              summarise(mean(steps))
daily_mean <- as.data.frame(daily_mean)
colnames(daily_mean) <- c("weekday", "interval", "steps")
# create a ggplot facet grid for activity pattern on weekdays and weekends.
p <- ggplot(daily_mean, aes(x=interval, y = steps)) + geom_line()

p + facet_grid(weekday ~ .)
```

![](PA1_template_files/figure-html/facet_grid-1.png)<!-- -->
