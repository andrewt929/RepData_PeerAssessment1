## Part 0: Require packagaes
require("knitr")
require("mice")
require("ggplot2")

## Part 1: Reading and Processing the data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
data <- read.csv(unz(temp, "activity.csv"), as.is = TRUE)
cleanData <- data[complete.cases(data), ]
unlink(temp)


## Part 2: Generating a histogram of the total number of steps taken each day
# Calculate the total number of daily steps
  dailySteps <- aggregate(steps ~ date, cleanData, sum)

# Create a histogram of no. of daily steps
  hist(dailySteps$steps, main = "Histogram of total number of daily steps", xlab = "Steps per day")

  
## Part 3: Calculating the mean & median
# Calculate the mean and median of the total number of daily steps
  round(mean(dailySteps$steps))

  median(dailySteps$steps)


## Part 4: Time series plot of avg number of steps taken
# Calculate average steps per interval for all days 
  avgIntervalSteps <- aggregate(steps ~ interval, cleanData, mean)

# Plot the time series with appropriate labels and heading
  plot(avgIntervalSteps$interval, avgIntervalSteps$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")


## Part 5: Identify the 5 minute interval that, on average, contains the maximum number of steps
# Identify the interval index which has the highest average steps
  interval_idx <- which.max(avgIntervalSteps$steps)

# Identify the specific interval and the average steps for that interval
  print (paste("The interval with the highest avg steps is ", avgIntervalSteps[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avgIntervalSteps[interval_idx, ]$steps, digits = 1)))


## Part 6: Code to describe & show a strategy for imputing missing data
# Devise a strategy & create a new dataset that is equal to the original but with the missing data filled in
  imputedData <- mice(data, m=5, method = "pmm", maxit = 3)
  completeData <- complete(imputedData, 1)
  

## Part 7: Generating a histogram of the total number of steps taken each day
# Calculate the total number of daily steps
  imputedDailySteps <- aggregate(steps ~ date, completeData, sum)

# Create a histogram of no. of daily steps
  hist(imputedDailySteps$steps, main = "Histogram of total number of daily steps (IMPUTED)", xlab = "Steps per day")

# Calculate the mean and median of the total number of daily steps
  round(mean(imputedDailySteps$steps))

  median(imputedDailySteps$steps)


## Part 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
  completeData['type_of_day'] <- weekdays(as.Date(completeData$date))
  completeData$type_of_day[completeData$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
  completeData$type_of_day[completeData$type_of_day != "weekend"] <- "weekday"

# convert type_of_day from character to factor
  completeData$type_of_day <- as.factor(completeData$type_of_day)

# calculate average steps by interval across all days
  df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, completeData, mean)

# creat a plot
  qplot(interval, 
        steps, 
        data = df_imputed_steps_by_interval, 
        type = 'l', 
        geom=c("line"),
        xlab = "Interval", 
        ylab = "Number of steps", 
        main = "") +
    facet_wrap(~ type_of_day, ncol = 1)