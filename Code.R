
#load data
unzip("activity.zip", "activity.csv")
data <- read.table("activity.csv", sep=",", header=T,stringsAsFactors = FALSE)


#convert to date
data$date<-as.Date(data$date,format="%Y-%m-%d")




#What is mean total number of steps taken per day?

#Make a histogram of the total number of steps taken each day

totals<-aggregate(data$steps, by=list(date=data$date), FUN=sum)
hist(totals$x,
     xlab="Total steps taken",
     main="Histogram of Total Steps Taken") 

#Calculate and report the mean and median total number of steps taken per day

#Means by day
daily_means<-aggregate(data[, 1], list(data$date), mean)
names(daily_means)<-c("Date","Average")
print(daily_means)

#mean all
all_mean<-mean(data[, 1],na.rm=TRUE)
print(all_mean)

#Median by day
daily_median<-aggregate(data[, 1], list(data$date), median)
names(daily_median)<-c("Date","Median")
print(daily_median)

#median all
all_median<-median(data[, 1],na.rm=TRUE)
print(all_median)


#What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
interval_means<-aggregate(data[, 1], list(data$interval), function(x) {mean(x,na.rm=TRUE)})
names(interval_means)<-c("Interval","int_means")
plot(interval_means$Interval,interval_means$int_means,type="l",
     ylab="Interval Means",
     xlab="Interval",
     main="Daily Activity Pattern")


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval_means[interval_means$int_means==max(interval_means$int_means),1]



#TAKE CARE OF MISSING






#Means <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
#data$steps <- apply(data, 1, function(r) ifelse(is.na(r[1]), Means[r[3]], r[1]))
#data$steps <- apply(data, 1, function(r) ifelse(is.na(r[1]), interval_means[interval_means$Interval==r[3],2], r[1]))


#Report number of rows with NA
length(data[is.na(data$steps),1])


#create a copy of the dataset to have missing values removed
rm_data<-data
m<-dim(rm_data)[1]

#replace missing values by averages for an interval
for (i in 1:m){
    if(is.na(rm_data[i,1])==TRUE)
    {rm_data$steps[i]<-interval_means[interval_means$Interval==rm_data[i,3],2]}
}    







#Make a histogram of the total number of steps taken each day
totals_rm<-aggregate(rm_data$steps, by=list(date=rm_data$date), FUN=sum)
hist(totals_rm$x,
     xlab="Total steps taken",
     main="Histogram of Total Steps Taken wo/Missing Values") 


#Calculate and report the mean and median total number of steps taken per day. 


#Means by day (no missing values)
daily_means_rm<-aggregate(rm_data[, 1], list(rm_data$date), mean)
names(daily_means_rm)<-c("Date","Average wo/Missing Values")
print(daily_means_rm)

#all_rm
all_mean_rm<-mean(rm_data[, 1],na.rm=TRUE)
print(all_mean_rm)

#Median by day (no missing values)
daily_median_rm<-aggregate(rm_data[, 1], list(rm_data$date), median)
names(daily_median_rm)<-c("Date","Median wo/Missing Values")
print(daily_median_rm)

#all_median_rm
all_median_rm<-median(rm_data[, 1],na.rm=TRUE)
print(all_median_rm)


#Compare with missing and wo/missing

#mean
cbind(daily_means,Average_woMissing=daily_means_rm[,2])

#median

cbind(daily_median,Median_woMissing=daily_median_rm[,2])

#total number of steps
sum(data$steps,na.rm =TRUE)
#total number of steps wo/Missing
sum(rm_data$steps,na.rm =TRUE)


#Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
rm_data$weekend_flag<-factor(ifelse(weekdays(rm_data$date,abbreviate = TRUE) %in% c("Sun","Sat"),"Weekend","Weekday"))

library(lattice)

weekend_flag_means<-aggregate(rm_data[, 1], list(rm_data$weekend_flag,rm_data$interval), mean)
names(weekend_flag_means)<-c("weekend_flag","interval","steps")

xyplot(steps~interval|weekend_flag,data=weekend_flag_means,layout=c(1,2),type="l",
       ylab="Number of steps",
       xlab="Interval")





