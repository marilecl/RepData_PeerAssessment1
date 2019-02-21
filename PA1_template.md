---
title: "Reproducible Research: Peer Assessment 1"
author: "Mario Leclerc"
date: "2019/02/17"
output: 
  html_document: 
    keep_md: true
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

* **steps:** Number of steps taking in a 5-minute interval (missing values are coded as ????????) </br>
* **date:** The date on which the measurement was taken in YYYY-MM-DD format </br>
* **interval:** Identifier for the 5-minute interval in which measurement was taken </br>

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 

## Loading and preprocessing the data
Unzip data to obtain a csv file.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)

unzip("activity.zip",exdir = "data")
activityDF <- read.csv("data/activity.csv",header = TRUE)
```

## What is mean total number of steps taken per day

1. Calculate the total number of steps taken per day


```r
StepsPerDay <- activityDF %>% group_by(date) %>% summarise(steps = sum(steps))
head(StepsPerDay,10)
```

```
## # A tibble: 10 x 2
##    date       steps
##    <fct>      <int>
##  1 2012-10-01    NA
##  2 2012-10-02   126
##  3 2012-10-03 11352
##  4 2012-10-04 12116
##  5 2012-10-05 13294
##  6 2012-10-06 15420
##  7 2012-10-07 11015
##  8 2012-10-08    NA
##  9 2012-10-09 12811
## 10 2012-10-10  9900
```

2. Make a histogram of the total number of steps taken each day. 


```r
ggplot(StepsPerDay, aes(x = steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Total Steps per day", x = "Number of Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
MeanStepsPerDay = mean(StepsPerDay$steps, na.rm = TRUE)
MedianStepsPerDay = median(StepsPerDay$steps, na.rm = TRUE)
```

Mean and median of the total number of steps taken per day are 1.076619\times 10^{4} steps and 10765 steps, respectively.

## What is the average daily activity pattern

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
MeanStepsPerInterval <- activityDF %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(steps = mean(steps))
ggplot(MeanStepsPerInterval, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Average daily activity pattern", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
MaxInterval <- MeanStepsPerInterval$interval[which.max(MeanStepsPerInterval$steps)]
MaxSteps <- max(MeanStepsPerInterval$steps)
```

Interval **"835"**, on average across all the days in the dataset, contains the maximum number of steps (**206.17 steps**).

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset 


```r
TotalMissingValues <- sum(is.na(activityDF$steps))
```

Total number of missing values in the dataset amounts to **2304 **.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# split activity data by interval
activityDF_ByInterval <- split(activityDF, activityDF$interval)
# fill in missing data for each interval with mean of respective interval
for(i in 1:length(activityDF_ByInterval))
{
    activityDF_ByInterval[[i]]$steps[is.na(activityDF_ByInterval[[i]]$steps)] <- MeanStepsPerInterval[i,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDF_Fill <- do.call("rbind", activityDF_ByInterval)
activityDF_Fill <- activityDF_Fill[order(activityDF_Fill$date) ,]
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
StepsPerDay <- activityDF_Fill %>% group_by(date) %>% summarise(steps = sum(steps))
ggplot(StepsPerDay, aes(x = steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Total Steps per day", x = "Number of Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# mean and median total number of steps taken per day
MeanStepsPerDay_Fill = mean(StepsPerDay$steps, na.rm = TRUE)
MedianStepsPerDay_Fill = median(StepsPerDay$steps, na.rm = TRUE)
```

Mean with NA was 1.0766189\times 10^{4} compared to 1.0766189\times 10^{4} without with imputed values.
Median with NA was 10765 compared to 1.0766189\times 10^{4}without with imputed values.
The median increased slightly. The method used to impute values had little impact on the estimates of the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
WeekDaysl = c(2,3,4,5,6) # 1=Sunday, 7=Saturday
activityDF_Fill <- activityDF_Fill %>% mutate(day = factor((wday(date) %in% WeekDaysl), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))
```

2. Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
MeanStepsPerInterval <- activityDF_Fill %>% filter(!is.na(steps)) %>% group_by(interval,day) %>% summarise(steps = mean(steps))
ggplot(MeanStepsPerInterval, aes(x = interval , y = steps, color=day)) + geom_line() + labs(title = "Average daily activity pattern across weekends and weekdays", x = "Interval", y = "Number of Steps")+ facet_wrap(~ day, nrow=2 )
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
