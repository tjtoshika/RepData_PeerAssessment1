---
title: "Course Project 1(Reproducible Research)"
output:
  html_document:
    keep_md: true
---

### Question 1. Code for reading in the dataset and/or processing the data.

Change the working directory where data is stored. Unzip the data and load the data in Rstudio.Steps given below - 


```r
unzip('repdata_data_activity.zip')
activity <- read.csv('activity.csv', colClasses = c('numeric', 'Date', 'numeric'))
```

Initial look of the data - 


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

### Question 2. What is the mean total number of steps taken per day?

As mentioned, for this part of assignment, we can ignore the missing values in the dataset.
Calculating the total number of steps taken per day -


```r
data <- aggregate(steps ~ date, data = activity, FUN = sum)
library(ggplot2)
```

Histogram of the total number of steps taken each day - 


```r
ggplot(data, aes(x=steps)) + geom_histogram(color='blue', fill="skyblue", bins = 20) + labs(title = 'Histogram of Total Number of Steps taken each day', x = 'Total Steps', y = 'Count')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Mean and median of the total number of steps taken per day - 


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
mean_median <- summarize(data, mean_steps = mean(steps), median_steps = median(steps))
mean_median
```

```
##   mean_steps median_steps
## 1   10766.19        10765
```

### Question 3. What is the average daily activity pattern ?




```r
average_steps <- activity %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(x = average_steps$interval, y = average_steps$mean_steps, type = 'l', main = 'Time Series Plot of Average Steps taken per Interval', xlab = 'Intervals(5 min)', ylab = 'Number of Steps', col = 'red', lwd = 1.5, family = 'serif')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

5-minute interval, which contains the maximum number of steps on average across all the days in the dataset - 


```r
average_step <- arrange(average_steps, desc(mean_steps))
average_step[1, ]
```

```
## # A tibble: 1 x 2
##   interval mean_steps
##      <dbl>      <dbl>
## 1      835       206.
```

The interval with the maximun number of steps is shown in the interval column above.

### Question 4. Imputing missing values.

The total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs) are -


```r
sum(rowSums(is.na(activity)))
```

```
## [1] 2304
```

Filling the missing values of the dataset with the mean for that 5 min interval  and creating a new dataset "activity1"- 


```r
activity1 <- activity
for (i in 1:dim(activity1)[1]){
        if (is.na(activity1[i, 1]) == TRUE ){
                activity1[i, 1] = average_steps[average_steps$interval %in% activity1[i,3], 2]
        }  
}
head(activity1)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Histogram of the total number of steps taken each day and mean, median of total number of steps taken per day - 


```r
total_steps_per_day <- aggregate(steps~date, data = activity1, FUN = sum)
ggplot(total_steps_per_day, aes(x=steps)) + geom_histogram(color='blue', fill="skyblue", bins = 20) + labs(title = 'Histogram of Total Number of Steps taken each day(Imputed Data)', x = 'Total Steps', y = 'Count')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_median_1 <- summarize(total_steps_per_day, mean_steps = mean(steps), median_steps = median(steps))
mean_median_1
```

```
##   mean_steps median_steps
## 1   10766.19     10766.19
```

Impact of imputing missing data on the estimates of the total daily number of steps -


```r
comparison <- rbind(mean_median, mean_median_1)
Dataset <- c('Dataset with Missing Values', 'Dataset with Imputed Values')
comparison <- cbind(comparison, Dataset)
col_order <- c('Dataset', 'mean_steps', 'median_steps')
comparison <- comparison[, col_order]
comparison
```

```
##                       Dataset mean_steps median_steps
## 1 Dataset with Missing Values   10766.19     10765.00
## 2 Dataset with Imputed Values   10766.19     10766.19
```

Also as seen from both the histograms of the dataset with missing values and dataset with imputed values, there is change in the frequency of the number of steps.


```r
na_grouping <- activity[is.na(activity$steps), c('date', 'steps')]
 table(na_grouping$date)
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288
```
So there were 8 full days with NA values, so the analysis without imputed values were done without considering these 8 days.

### Question 5. Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day - 


```r
day_of_week <- weekdays(activity1$date)
day_of_week_new <- c()
for (i in 1:dim(activity1)[1]){
        if (day_of_week[i] %in% c('Saturday', 'Sunday')){
                day_of_week_new[i] = 'Weekend'
        }
        else{
                day_of_week_new[i] = 'Weekday'
        }
}
activity1$day <- day_of_week
activity1$day_in_week <- day_of_week_new
activity1$day_in_week <- as.factor(activity1$day_in_week)
activity1 <- activity1[-c(4)]
str(activity1)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date       : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval   : num  0 5 10 15 20 25 30 35 40 45 ...
##  $ day_in_week: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
head(activity1)
```

```
##       steps       date interval day_in_week
## 1 1.7169811 2012-10-01        0     Weekday
## 2 0.3396226 2012-10-01        5     Weekday
## 3 0.1320755 2012-10-01       10     Weekday
## 4 0.1509434 2012-10-01       15     Weekday
## 5 0.0754717 2012-10-01       20     Weekday
## 6 2.0943396 2012-10-01       25     Weekday
```

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days-


```r
data_day_in_week <- activity1 %>% group_by(day_in_week, interval) %>% summarize(mean_steps = mean(steps))
```

```
## `summarise()` regrouping output by 'day_in_week' (override with `.groups` argument)
```

```r
qplot(x = interval, y = mean_steps, data = data_day_in_week, facets = ~day_in_week, geom = c('line'), xlab = '5-min interval', ylab = 'Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
