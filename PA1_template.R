library(zoo)
library(ggplot2)
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer","character","integer"))
#------------------------------------------------------------
data$date <- as.Date(data$date)
sum_data <- aggregate(steps ~ date, data = data, FUN = sum)  
qplot(steps, data = sum_data, binwidth = 1000, geom = "histogram",
      xlab = "Number of steps per day", ylab = "Frequency in a bin (count)")
mean_data <- mean(sum_data$steps)
median_data <- median(sum_data$steps)
#------------------------------------------------------------ 
ave_interval <- aggregate(steps ~ interval,data = data, FUN = mean)
sort_ave_interval <- ave_interval[order(ave_interval$interval),]
time_sequence <- seq(from = as.POSIXct("2012-01-10 00:00"), to = as.POSIXct("2012-01-10 23:55"), by = 300)
ts_plot <- zoo(sort_ave_interval$steps,time_sequence)
plot(ts_plot, xlab = "5-minute interval", 
     ylab = "Mean number of steps across all days")
#------------------------------------------------
sapply(data, function(x) sum(is.na(x)))
copy_data <- data
na_value <- data[which(is.na(data)),]
na_value$steps <- rep(sort_ave_interval$steps,8)
copy_data[which(is.na(copy_data)),] <- na_value
#----------------------------------------------------------
new_sum_data <- aggregate(steps ~ date, data = copy_data, FUN = sum)  
qplot(steps, data = new_sum_data, binwidth = 1000, geom = "histogram",
      xlab = "Number of steps per day", ylab = "Frequency in a bin (count)")
new_mean_data <- mean(new_sum_data$steps)
new_median_data <- median(new_sum_data$steps)
print(new_mean_data)
print(new_median_data)
#--------------------------------------------------
wd <- weekdays(copy_data$date)
for(i in 1:length(wd)){
    if(wd[i] == "Saturday" | wd[i] == "Sunday"){
        wd[i] = "weekend"
    }
    else{
        wd[i] = "weekday"
    }
}    
copy_data$weekdate <- wd
c_data <- aggregate(steps ~ interval, data = copy_data, FUN = mean)
new_data <- split(copy_data, copy_data$weekdate )
weekday_interval <- aggregate(steps ~ interval,data = new_data$weekday, FUN = mean)
weekday_interval$weekdate <- "weekday" 
weekend_interval <- aggregate(steps ~ interval,data = new_data$weekend, FUN = mean)
weekend_interval$weekdate <- "weekend" 
to_plot <- rbind(weekday_interval,weekend_interval)
ggplot(to_plot, aes(x=interval, y=steps)) + 
  geom_line(color="violet") + 
  facet_wrap(~ weekdate, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()

