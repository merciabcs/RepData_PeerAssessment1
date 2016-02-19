# Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
data_orig <- read.csv('activity.csv')

# Process/transform the data (if necessary) into a format suitable for your analysis
data_orig$date <- as.Date(data_orig$date)
summary(data_orig)
data_clean <- data_orig[complete.cases(data_orig),]
head(data_clean,5)


# Calculate the total number of steps taken per day
steps_sum <- aggregate(steps ~ date, data_clean, sum)
head(steps_sum,5)

# If you do not understand the difference between a histogram and a barplot, research the difference between them. 
# Make a histogram of the total number of steps taken each day
hist(steps_sum$steps, breaks=10, main='Histogram of Total number of steps taken each day', 
     xlab='Total number of steps per day', col='red')

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(steps_sum$steps)
median_steps <- median(steps_sum$steps)

# Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg_steps_5min <- aggregate(steps ~ interval, data_clean, mean)
plot(avg_steps_5min$interval, avg_steps_5min$steps, type='l', col='red',
     main='Time Series - Average number of steps taken each day',
     xlab='Time (minutes)', ylab='Average number of steps')

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_5min <- avg_steps_5min[avg_steps_5min$steps==max(avg_steps_5min$steps),'interval']

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
total_na <- sum(is.na(data_orig))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## EXPLAIN EXTRATEGY

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_filled <- data_orig
for (i in 1:nrow(data_filled)) {
    if (is.na(data_filled[i,'steps'])) {
        data_filled[i,'steps']<- avg_steps_5min[avg_steps_5min$interval==data_filled[i,'interval'],'steps']
    }
}

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
steps_sum_filled <- aggregate(steps ~ date, data_filled, sum)
hist(steps_sum_filled$steps, breaks=10, main='Histogram of Total number of steps (filled data)', 
     xlab='Total number of steps per day', col='red')

mean_steps_filled <- mean(steps_sum_filled$steps)
median_steps_filled <- median(steps_sum_filled$steps)


# For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help her
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

data_days <- data_filled
data_days$day <- weekdays(data_days$date)

for (i in 1:nrow(data_days)) {
    if (data_days[i,'day']=='Saturday' | data_days[i,'day']=='Sunday') {
        data_days[i,'day'] <- 'Weekend'
    }
    else {
        data_days[i,'day'] <- 'Weekday'
    }
}

data_days$day <- factor(data_days$day)

# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = 
# "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
# See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

library(lattice)
avg_steps_5min_day <- aggregate(steps ~ interval + day, data_days, mean)
xyplot(steps ~ interval | day, avg_steps_5min_day, type='l',layout=c(1,2),
       main='Average Steps Taken', xlab='Time (minutes)', ylab='Average number of steps')


