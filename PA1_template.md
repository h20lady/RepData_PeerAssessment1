---
title: "PA1_template"
author: "H20Lady"
date: "Saturday, October 11, 2014"
output: html_document

---

## Assignment
****


```r
#set working directory in R to folder with database files
act <- read.csv("activity.csv")
head(act)
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
# Format Date column in R
act$dateconverted <- as.Date(act$date, format="%Y-%m-%d")

# Create a data frame grouped by Day column and Summed by Steps column

library(dplyr)

act1 <- 
     act %>%
     group_by(dateconverted) %>%
     summarise(totsteps = sum(steps, na.rm=TRUE))
```

****
### What is mean total number of steps taken per day?
##### Histogram of the total number of steps taken each day. 
###### Missing values have been ignored (i.e. removed) in this dataset.
****

```r
library(ggplot2)

p <- ggplot(act1, aes(x=totsteps)) + geom_histogram(binwidth=1000)

hist<- p + 
     labs(title = "Histogram of Total Number of Steps Per Day\n Missing Values Included") + 
     xlab("Total Number of Steps Per Day") + ylab("Frequency of Total Number of Steps Per Day") + 
     xlim(0,25000) +
     ylim(0, 20)

print(hist)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
#Saves as image file to drive:
#ggsave("Fig1_act.png", plot=hist, scale = 1, width = 8, height = 5, units = c("in"), dpi = 80)
```

****
##### Mean and Median of Total number of steps taken per day. 
###### Missing values have been ignored in this dataset.
****

```r
mean(act1$totsteps)
```

```
## [1] 9354.23
```

```r
median(act1$totsteps)
```

```
## [1] 10395
```
Mean of Total Steps Per Day = 9354 and Median of Total Steps Per Day = 10395

****
### What is the average daily activity pattern?
##### Time-Series Plot
****

```r
#find the mean of each interval while ignoring NAs

act2 <- 
     act %>%
     group_by(interval) %>%
     summarise(avgsteps = mean(steps,na.rm=TRUE))


q <- ggplot(act2, aes(x=interval, y=avgsteps)) + geom_line(colour = "red", size = 0.5)

hist1<- q + 
     labs(title = "Time Series Plot of 5 minute-intervals \n From October 1, 2012 to November 30, 2012 \n Missing Values Ignored")+ 
     xlab("Intervals") + ylab("Avg. Number of Steps") + 
     xlim(0,3000) +
     ylim(0, 300)

print(hist1)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#Saves as image file to drive:
#ggsave("Fig2_act.png", plot=hist, scale = 1, width = 8, height = 5, units = c("in"), dpi = 80)

which.max(act2$avgsteps)
```

```
## [1] 104
```

```r
act2[104,]
```

```
## Source: local data frame [1 x 2]
## 
##     interval avgsteps
## 104      835 206.1698
```

The **835th** interval contains the maximum average number of steps

****
### Imputing missing values
##### Histogram of the total number of steps taken each day. 
###### This dataset has missing values filled.
****

```r
summary(act)
```

```
##      steps                date          interval      dateconverted       
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0   Max.   :2012-11-30  
##  NA's   :2304     (Other)   :15840
```

There are 2304 NAs in the dataset

****
###### This dataset has missing values filled.
****
The strategy is to find NA values for each interval and replace with mean of that interval.


```r
#find NA values for each interval and replace with mean of that interval

act_xna_filled <- 
     
     act %>% 
     group_by(interval) %>% 
     mutate(meanstepsNew = ifelse(is.na(steps), mean(steps,na.rm=TRUE), steps))

act_xna_filled2 <-
     
     act_xna_filled %>%
     group_by(dateconverted) %>%
     summarise(totstepswithNA = sum(meanstepsNew))
```

****

```r
mean(act_xna_filled2$totstepswithNA)
```

```
## [1] 10766.19
```

```r
median(act_xna_filled2$totstepswithNA)
```

```
## [1] 10766.19
```

The mean and median from the previous part of the assignment were 9354 and 10395 respectively.
Based on the mean and medians above, the 2304 rows of NAs do impact the total average daily number of steps.

****

```r
r <- ggplot(act_xna_filled2, aes(x=totstepswithNA)) + geom_histogram(binwidth=1000)

hist<- r + 
     labs(title = "Histogram of Total Number of Steps Per Day\n Missing NA Values Filled") + 
     xlab("Total Number of Steps Per Day") + ylab("Frequency of Total Number of Steps Per Day") + 
     xlim(0,25000) +
     ylim(0,20)

print(hist)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
#Saves as image file to drive:
#ggsave("Fig3_act.png", plot=hist, scale = 1, width = 8, height = 5, units = c("in"), dpi = 80)
```

### Are there differences in activity patterns between weekdays and weekends?


```r
#create a new factor variable for weekday
mtwtf <- function(day) {
  wday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  is.element(weekdays(as.Date(day)), wday)
}

# create a new column "daytype" and store in a new data frame called "act_mtwtf"
act_mtwtf <-act_xna_filled %>% 
     mutate(daytype = factor(mtwtf(date), levels=c(FALSE, TRUE), labels=c("Weekend", "Weekday")))

meansteps_withlabels <- 
     act_mtwtf %>% 
     group_by(interval, daytype) %>% 
     summarize(steps=mean(meanstepsNew))
```



```r
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


s <- qplot(interval, steps, data=meansteps_withlabels, geom=c("line"), facets= daytype ~ ., main="Time Series Plot with Missing Values Filled \n (Weekday/Weekend)", xlab="Interval", ylab="Average Steps Taken")

print(s)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
#Saves as image file to drive:
#ggsave("Fig4_act.png", plot=hist, scale = 1, width = 8, height = 5, units = c("in"), dpi = 80)
```

Yes. There are differences between weekend and weekday activities.
