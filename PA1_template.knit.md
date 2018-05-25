---
title: "Reproducible Research - Week 2 Course Project"
author: "Sanjay Lonkar"
date: "25 May 2018"
output: html_document
---



## Analyzing Fitbit Data

### Introduction 
This project is an assignment of **Reproducible Research** course of **Data Science Specialization**. It aims to complete entire assignment in a single R Markdown document that can be processed by knitr and be transformed into an HTML file. 

### Data
Data for this project can be downloaded from course website:

* Data [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken


The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading of Data

```r
if (!file.exists("./downloadedDataset"))
{
  dir.create("./downloadedDataset")
}
if (!file.exists ("./downloadedDataset/downloadedDataset.zip")) # This step is to avoid downloading data every time one runs this script
{
  datasetURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file (datasetURL, destfile="./downloadedDataset/downloadedDataset.zip")
  unzip (zipfile = "./downloadedDataset/downloadedDataset.zip", exdir="./downloadedDataset")
}

dataSet <- read.csv (file="./downloadedDataset/activity.csv", head = TRUE, sep = ",")
```

### What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
perDaySteps <- tapply (dataSet$steps, dataSet$date, sum, na.rm = TRUE)

# Make a histogram of the total number of steps taken each day
library(ggplot2)
p <- qplot (perDaySteps, geom="histogram", main = 'Total Number of Steps Per Day', 
            fill=I("blue"), col=I("red"), alpha=I(.2),
            xlab = "Day", ylab = 'Frequency using binwith 500', binwidth=500)
print(p)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
# Calculate and report the mean and median of the total number of steps taken per day
perDayStepsMean <- mean (na.omit(perDaySteps))
perDayStepsMedian <- median (na.omit(perDaySteps))

print(paste("Mean is ", perDayStepsMean))
```

```
## [1] "Mean is  9354.22950819672"
```

```r
print(paste("Median is ", perDayStepsMedian))
```

```
## [1] "Median is  10395"
```

### What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageStepsPerTimeBlock <- aggregate (x = list (meanSteps = dataSet$steps), by = list (interval = dataSet$interval), FUN = mean, na.rm = TRUE)

# Make a time series plot
p <- ggplot(data = averageStepsPerTimeBlock, aes(x = interval, y = meanSteps)) +
          geom_line() +
          xlab ("5-minute interval") +
          ylab ("Average number of steps taken") 
print(p)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" width="672" />

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mostSteps <- which.max (averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
print(paste("Average maximum number of steps ", mostSteps, " for the interval ", timeMostSteps))
```

```
## [1] "Average maximum number of steps  104  for the interval  8:35"
```

### Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
numMissingValues <- length (which (is.na (dataSet$steps)))
print(paste("Number of missing values prior to imputing is", numMissingValues))
```

```
## [1] "Number of missing values prior to imputing is 2304"
```

```r
index <- which (is.na (dataSet$steps))
l <- length (index)
steps_avg <- with (dataSet, tapply (steps, date, mean, na.rm = TRUE))
na <- mean (steps_avg, na.rm = TRUE)
dataSetImputed <- dataSet
for (i in 1 : l) {
  dataSetImputed [index[i], 1] <- na
}
perDayStepsImputed <- tapply (dataSetImputed$steps, dataSetImputed$date, sum, na.rm = TRUE)

p <- qplot (perDayStepsImputed, geom="histogram", main = 'Total Number of Steps Per Day (After Imputing NAs)', 
            fill=I("blue"), col=I("red"), alpha=I(.2),
            xlab = "Day", ylab = 'Frequency using binwith 500', binwidth=500)
print(p)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```r
print (paste("Mean after imputing is ", mean(perDayStepsImputed), "Mean prior to imputing was ", perDayStepsMean, "Difference is ", mean(perDayStepsImputed) - perDayStepsMean))
```

```
## [1] "Mean after imputing is  10766.1886792453 Mean prior to imputing was  9354.22950819672 Difference is  1411.95917104856"
```

```r
print (paste("Median after imputing is ", median(perDayStepsImputed), "Median prior to imputing was ", perDayStepsMedian, "Difference is ", median(perDayStepsImputed) - perDayStepsMedian))
```

```
## [1] "Median after imputing is  10766.1886792453 Median prior to imputing was  10395 Difference is  371.188679245282"
```

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
weekdays <- c ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dataSet$dow = as.factor (ifelse (is.element (weekdays (as.Date(dataSet$date)), weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, dataSet, mean)

library (lattice)

xyplot (steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main = "Average Steps per Day by Interval", xlab = "Interval", ylab = "Steps",layout = c (1, 2), type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" width="672" />















