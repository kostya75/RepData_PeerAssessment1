# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip", "activity.csv")
data <- read.table("activity.csv", sep=",", header=T,stringsAsFactors = FALSE)

data$date<-as.Date(data$date,format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?


###Make a histogram of the total number of steps taken each day

```r
totals<-aggregate(data$steps, by=list(date=data$date), FUN=sum)
hist(totals$x,
     xlab="Total steps taken",
     main="Histogram of Total Steps Taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


###Calculate and report the mean and median total number of steps taken per day
Mean

```r
all_mean<-mean(data[, 1],na.rm=TRUE)
print(all_mean)
```

```
## [1] 37.3826
```
Median

```r
all_median<-median(data[, 1],na.rm=TRUE)
print(all_median)
```

```
## [1] 0
```


## What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
interval_means<-aggregate(data[, 1], list(data$interval), function(x) {mean(x,na.rm=TRUE)})
names(interval_means)<-c("Interval","int_means")
plot(interval_means$Interval,interval_means$int_means,type="l",
     ylab="Interval Means",
     xlab="Interval",
     main="Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval<-interval_means[interval_means$int_means==max(interval_means$int_means),1]
print(max_interval)
```

```
## [1] 835
```


## Imputing missing values


###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(data[is.na(data$steps),1])
```

```
## [1] 2304
```


###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


###Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
rm_data<-data
m<-dim(rm_data)[1]

#replace missing values by averages for an interval
for (i in 1:m){
    if(is.na(rm_data[i,1])==TRUE)
    {rm_data$steps[i]<-interval_means[interval_means$Interval==rm_data[i,3],2]}
} 
```


###Make a histogram of the total number of steps taken each day

```r
totals_rm<-aggregate(rm_data$steps, by=list(date=rm_data$date), FUN=sum)
hist(totals_rm$x,
     xlab="Total steps taken",
     main="Histogram of Total Steps Taken wo/Missing Values") 
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


###Calculate and report the mean and median total number of steps taken per day

Mean

```r
all_mean_rm<-mean(rm_data[, 1],na.rm=TRUE)
print(all_mean_rm)
```

```
## [1] 37.3826
```
Median

```r
all_median_rm<-median(rm_data[, 1],na.rm=TRUE)
print(all_median_rm)
```

```
## [1] 0
```


###Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Estimate of the total daily number is unchanges as I'm replacing missing values with mean


## Are there differences in activity patterns between weekdays and weekends?


###Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
rm_data$weekend_flag<-factor(ifelse(weekdays(rm_data$date,abbreviate = TRUE) %in% c("Sun","Sat"),"Weekend","Weekday"))
```

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.2
```

```r
weekend_flag_means<-aggregate(rm_data[, 1], list(rm_data$weekend_flag,rm_data$interval), mean)
names(weekend_flag_means)<-c("weekend_flag","interval","steps")

xyplot(steps~interval|weekend_flag,data=weekend_flag_means,layout=c(1,2),type="l",
       ylab="Number of steps",
       xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 


