# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Task:
1. Code for reading in the dataset and/or processing the data

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "datafile.zip")

unzip("datafile.zip")

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
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
act_tbl <- read.csv("activity.csv", stringsAsFactors = FALSE, header = TRUE)
act_tbl$date <- as.Date(act_tbl$date)
```

```
## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
## 'zone/tz/2018c.1.0/zoneinfo/Europe/Copenhagen'
```

```r
act_tbl$teps <- as.numeric(act_tbl$steps)
```

## What is mean total number of steps taken per day?

Task:
2. Histogram of the total number of steps taken each day  
Showing the number of steps in histogram gives an indication on the distribution of steps  

```r
library(dplyr)
hist_data <- act_tbl %>% group_by(date) %>% summarise(steps = sum(steps))
#Plotting out the histogram - I have chosen to show 30 bins
hist(hist_data$steps, main = "Distribution af steps pr date", xlab = "Number of steps", breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
Task:
3. Calculate and report the mean and median of the total number of steps taken per day  

```r
mean_steps = mean(hist_data$steps, na.rm = TRUE)
median_steps = median(hist_data$steps, na.rm = TRUE)
```

* The mean of total number of steps taken pr day is  

```r
print(mean_steps)
```

```
## [1] 10766.19
```
* The median of total number of steps taken per day is  

```r
print(median_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Task:
4. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
int_steps <- act_tbl %>% 
                group_by(interval) %>%
                summarise(avg_steps = mean(steps, na.rm = TRUE))
plot(int_steps, type = "l", main = "Average number of steps pr interval", xlab = "5-min interval", ylab = "Average number of steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Task 
5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

We will find the max of the previous created dataset used for the plot  

```r
max_int <- int_steps[int_steps$avg_steps == max(int_steps$avg_steps),]
print(max_int)
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

The calculation shows that the interval with maximum number of steps is 835, where the average number of steps is 206


## Imputing missing values

Task 
6. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
We will find the missing values with the count of a is.na command  

```r
no_miss <- sum(is.na(act_tbl$steps))
print(no_miss)
```

```
## [1] 2304
```

So the number of rows with NA in the measures steps is 2304  

7. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For exampple, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

As proposed by the author I will use the average of the 5 minute interval for the missing values in each 5 minute interval. A better approach might be to use some sort of nearest neighbour approach, but there are to many missing valueas and I am not sure that each participant is comparable in that sense, so I wil fill out each missing value with the mean of that interval.  

8. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(dplyr)
new_data <- act_tbl %>%
                left_join(int_steps, by = 'interval') %>%
                mutate(steps = ifelse(is.na(steps), avg_steps, steps))
```

9. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
hist_data_2 <- new_data %>% group_by(date) %>% summarise(steps = sum(steps))
#Plotting out the histogram without missing values

hist(hist_data_2$steps, main = "Distribution af steps pr date - without missing values", xlab = "Number of steps", breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

There is a clear difference in the distribution now, with the spike more obvious.


```r
mean_steps_no_miss = mean(hist_data_2$steps, na.rm = TRUE)
median_steps_no_miss = median(hist_data_2$steps, na.rm = TRUE)
```

The median of the steps with NA values was 10766, without missing values    

```r
print(mean_steps_no_miss)
```

```
## [1] 10766.19
```
And the mean witn NA values was 10765, without

```r
print(median_steps_no_miss)
```

```
## [1] 10766.19
```

So no big diffrence.  

## Are there differences in activity patterns between weekdays and weekends?

Tasks
10. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

We will start with creating a vector containing weekdays

```r
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
```

Then use an ifelse staemant to check whether a given day is part of that vector, if not the weekend


```r
new_data$wkd <- as.factor(ifelse(weekdays(new_data$date) %in% weekdays, 'weekday', 'weekend'))
```

11. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

First we will create the dataset for the average

```r
int_steps_2 <- aggregate(steps ~ interval + wkd, new_data, mean)
```


The the plot


```r
library(lattice)

xyplot(steps ~ interval | wkd, data = int_steps_2, main="Average Steps 
       per Day", xlab = "Interval", ylab = "Steps", layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
