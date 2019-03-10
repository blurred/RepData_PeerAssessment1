---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
### Jason Seril
#### March 7, 2019

## Loading and preprocessing the data

```r
knitr::opts_chunk$set(warning=FALSE)
library(ggplot2)
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)

summary(activity)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```

## What is mean total number of steps taken per day?

```r
activityTotalSteps <- with(activity, aggregate(steps, by=list(date), FUN=sum, 
                                                 na.rm=TRUE))
names(activityTotalSteps) <- c("date","steps")
hist(activityTotalSteps$steps, main="Total number of steps taken per day",
     xlab="Total steps taken per day", col="darkblue", ylim=c(0,20), 
     breaks=seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean of the total number of steps taken per day:

```r
mean(activityTotalSteps$steps)
```

```
## [1] 9354.23
```
The median of the total number of steps taken per day:

```r
median(activityTotalSteps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
averageDailyActivity <- aggregate(activity$steps, by=list(activity$interval),
                                  FUN=mean, na.rm=TRUE)
names(averageDailyActivity) <- c("interval","mean")
plot(averageDailyActivity$interval, averageDailyActivity$mean, type="l",col="darkblue",
     lwd=2, xlab="Interval", ylab="Average number of steps", 
     main="Average number per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageDailyActivity[which.max(averageDailyActivity$mean),]$interval
```

```
## [1] 835
```


## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
imputedSteps <- averageDailyActivity$mean[match(activity$interval, 
                                                averageDailyActivity$interval)]
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityImputed <- transform(activity, steps=ifelse(is.na(activity$steps), 
                                                    yes=imputedSteps, 
                                                    no=activity$steps))
totalStepsImputed <- aggregate(steps ~ date, activityImputed, sum)
names(totalStepsImputed) <- c("date", "daily_steps")
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(totalStepsImputed$daily_steps, col="darkblue", xlab="Total steps per day",
     ylim=c(0,30), main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

