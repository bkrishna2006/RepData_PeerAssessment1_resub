---
title: "ActivityDataAnalysis"
author: "Balaji K"
date: "7 January 2018"
output: 
  html_document: 
    keep_md: yes
---



## R Markdown

1. Read the zip file with read.csv 
(Note: Direct download from R script failed due to connection issues; Error message --> curl: (7) Failed to connect to d396qusza40orc.cloudfront.net port 443: Connection timed out
download had nonzero exit status>.     Hence, downloaded from browser.)

2. Include dplyr package for data manipulations
5. Load the data into a tbl_df and arrange columns, group by and summarize
6. Now, get answer for Q1 - What is mean total number of steps taken per day?

Loading and preprocessing the data



```r
#sourcefileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#destfilename <- "activity.zip"
#myRawData <- download.file(url = sourcefileURL, destfile = destfilename, method = "curl")
#myRawData <- read.csv(destfilename)

myRawData <- read.csv(unzip("activity.zip","activity.csv"))
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
myRawData$date <- as.Date(myRawData$date, format = "%Y-%m-%d")
my_tib1 <- tbl_df(myRawData)
my_tib2 <- select(.data = my_tib1, 2:3, 1)
by_date <- group_by(my_tib2, date)
by_date_smry <- summarise(by_date, steps_tot = sum(steps,na.rm = TRUE))
# summary(by_date_smry)
```
#  Plot the histogram on Daily steps count

```r
plot(by_date_smry, type = "h", lwd = 5, col = "blue", lend = "square", ylab = "Steps", xlab = "Date", main = "Total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Code to calculate Mean and median number of steps taken each day

```r
mean_steps_per_day <- mean(by_date_smry$steps_tot)
median_steps_per_day <- median(by_date_smry$steps_tot)
```
### Mean of steps taken per day:
### Median of steps taken per day:


```r
mean_steps_per_day
```

```
## [1] 9354.23
```

```r
median_steps_per_day
```

```
## [1] 10395
```

#  Time series plot of the average number of steps taken


```r
plot(aggregate(steps ~ interval, data = by_date, FUN = mean), type = "l", col ="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###  The 5-minute interval that, on average, contains the maximum number of steps


```r
by_interval <- group_by(my_tib2, interval)
by_interval_smry <- summarise(by_interval, max_steps = max(steps, na.rm = TRUE))
arrange(by_interval_smry, desc(max_steps))
```

```
## # A tibble: 288 x 2
##    interval max_steps
##       <int>     <dbl>
##  1      615       806
##  2      900       802
##  3      550       794
##  4      720       789
##  5      835       786
##  6      925       785
##  7     1600       785
##  8     1635       785
##  9     1140       783
## 10      850       781
## # ... with 278 more rows
```

```r
maximum_steps <- max(by_interval_smry$max_steps)
temp <- filter(by_interval_smry, max_steps == maximum_steps)
```

### The interval that on a daily average had the max number of steps =


```r
temp$interval
```

```
## [1] 615
```

Code to describe and show a strategy for imputing missing data
Replaced NAs with mean value of steps


```r
by_date_cleaned <- by_date
by_date_cleaned$steps[is.na(by_date_cleaned$steps)] <- mean(na.omit(by_date_cleaned$steps))
```

Histogram of the total number of steps taken each day after missing values are imputed
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
by_date_smry <- aggregate(steps ~ date, rm.na = TRUE, data = by_date, FUN = sum)
by_date_smry_cleaned <- aggregate(steps ~ date, rm.na = TRUE, data = by_date_cleaned, FUN = sum)

par(mfrow = c(1, 2)) 
plot(by_date_smry, type = "h", lwd = 2, col = "blue", lend = "square", ylab = "Steps", xlab = "Date", main = "with NAs")
plot(by_date_smry_cleaned, type = "h", lwd = 2, col = "green", lend = "square", ylab = "Steps", xlab = "Date", main = "with NAs replaced")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### It is observed that replacing NAs suitably has brought continuity to the plots, which means the measurements are better comparable !


```r
mean_by_date <- aggregate(steps ~ date, data = by_date, FUN = mean)
mean_by_date_cleaned <- aggregate(steps ~ date, data = by_date_cleaned, FUN = mean)
median_by_date <- aggregate(steps ~ date, data = by_date, FUN = median)
median_by_date_cleaned <- aggregate(steps ~ date, data = by_date_cleaned, FUN = median)
```
1. Mean of steps when data had NAs
2. Mean of steps when data did not have NAs
3. Median of steps when data had NAs
4. Median of steps when data did not have NAs


```r
mean(mean_by_date$steps)
```

```
## [1] 37.3826
```

```r
mean(mean_by_date_cleaned$steps)
```

```
## [1] 37.3826
```

```r
mean(median_by_date$steps)
```

```
## [1] 0
```

```r
mean(median_by_date_cleaned$steps)
```

```
## [1] 4.902636
```


Are there differences in activity patterns between weekdays and weekends?


```r
by_date_cleaned$date <- as.POSIXct(by_date_cleaned$date)
by_date_cleaned$daytype <-  ifelse(weekdays(by_date_cleaned$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
by_date_cleaned$daytype <- as.factor(by_date_cleaned$daytype)
```
Plotting the timeseries plots


```r
q <- by_date_cleaned %>% group_by(daytype, interval) %>% summarize(daily_steps_tot = sum(steps))

library(lattice)
with(q, {
      xyplot(daily_steps_tot ~ interval | daytype, 
      type = "l",    
      col = "red",
      main = "Activities - Weekdays Vs. Weekend",
      xlab = "5-min Intervals",
      ylab = "Steps count")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


### From the comparative time-series plot, it is evident that there is more activity during weekdays compared to weekends.


