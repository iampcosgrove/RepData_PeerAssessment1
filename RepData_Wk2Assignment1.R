## Working filefor Coursera Reproducible Data, Week 1 Assignment.



# Code for reading in the dataset and/or processing the data

# Set working environment

setwd("~/GitHub/RepData_PeerAssessment1")

# Load required packages

library(data.table)
library(ggplot2)
library(dplyr)
library(knitr)

 # Download and read data file

if(!file.exists("data")){
      dir.create("data")
}

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists("~/GitHub/RepData_PeerAssessment1/")) {
      download.file(fileURL, dest= "./data/dataZIP.zip", mode= "wb")
      unzip("~/GitHub/RepData_PeerAssessment1/data/dataZIP.zip", 
            exdir = "~/GitHub/RepData_PeerAssessment1/data")
      file.remove("~/GitHub/RepData_PeerAssessment1/data/dataZIP.zip")}

activity <- fread("~/GitHub/RepData_PeerAssessment1/data/activity.csv")

# Remove NA's as they will impact statistics by including only complete cases
activity_clean <- activity[complete.cases(activity),]

#Question Set # 1- What is mean total number of steps taken per day?

# 1A- Calculate the total number of steps taken per day

TotalStepsDay <- activity_clean %>% group_by(date) %>% summarise(TotalSteps = sum(steps))

# 1B- Make a histogram of the total number of steps taken each day

ggplot(TotalStepsDay, aes(x= date, y= TotalSteps))+
      geom_histogram(stat= 'identity', col= "black", fill= "blue2")+
      labs(x= "Date", y= "Total Steps per Day", title= "Monitored Steps per Day")
     
# 1C- Calculate and report the mean and median of the total number of steps taken per day

StepsMean <- mean(TotalStepsDay$TotalSteps)
StepsMedian <- median(TotalStepsDay$TotalSteps)

      ##FOR Markdown##  
# The mean steps/day is 'StepsMean', while the median steps/day is 'StepsMedian'.

# Question Set # 2- Imputting missing Values

# 2A- Calculate and report the total number of missing values in the dataset (i.e. the total number 
#      of rows with NAs)

sum(is.na(activity$steps))


# 2B- Devise a strategy for filling in all of the missing values in the dataset. The strategy does 
#       not need to be sophisticated. For example, you could use the mean/median for that day, or the 
#       mean for that 5-minute interval, etc.

# Calculate average steps per interval period across all days
AvgStepsInterval <- activity_clean %>% group_by(interval) %>% summarise(steps = mean(steps))

# Let's take a look and plot Average steps per interval over the course of the day
ggplot(AvgStepsInterval)+ 
   geom_line(aes(interval, steps), color = "Blue", size = 1)+
   labs(x= "Time of Day", y= "Average Steps/5min", title= "Average Steps Over A Day")

# 2B- Create a new dataset that is equal to the original dataset but with the missing data filled in.

# First, we'll replace NA's of missing steps with mean steps of that specific interval.  This is due to missing
# data for 8 days.  This will be the same as replacing the missing days with the mean total sum of those days.


Activity_Imput <- activity %>% inner_join(AvgStepsInterval, by= "interval") %>%
   mutate(steps = coalesce(steps.x, steps.y)) %>%
   select(date, interval, steps)

# After replacing missing intervals we need to calculate the total steps per day for each day.

ImputTotalStepsDay <- Activity_Imput %>% group_by(date) %>% summarise(TotalSteps = sum(steps))
ImputStepsMean <- mean(ImputTotalStepsDay$TotalSteps)
ImputStepsMedian <- median(ImputTotalStepsDay$TotalSteps)

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
# median total number of steps taken per day. Do these values differ from the estimates from the first 
# part of the assignment? What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?

ggplot(ImputTotalStepsDay, aes(x= date, y= TotalSteps))+
   geom_histogram(stat= 'identity', col= "black", fill= "blue2")+
   labs(x= "Date", y= "Total Steps per Day", title= "Imputed Monitored Steps per Day")

##FOR Markdown##  
# The mean steps/day is 'ImputStepsMean', while the median steps/day is 'ImputStepsMedian'.

# Based on these results the mean and median did not change, compared to initial calculations in the 
# first section inidcating that data was correctly cleaned in the first section.  If the numbers had changed
# it would have indicated that errors from the NA were contributing to calculations of the mean/median.
# However, in general imputting the missing data can potentially falsely affirm observed values.


# Question Set # 3- Are there differences in activity patterns between weekdays and weekends?

#3A- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating 
#     whether a given date is a weekday or weekend day.

# First, we will use the cleaned activity dataset (activity_clean). To add day of the week and convert
# to weekday (Monday - Friday) or weekend (Saturday - Sunday).

activity_clean$Day <- weekdays(as.Date(activity_clean$date))
activity_clean$Day <- as.factor(activity_clean$Day)
levels(activity_clean$Day) <-list(
   Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
   Weekend = c("Saturday", "Sunday")
)

#3B- Make a panel plot containing a time series plot (i.e. type = "l") 
#     of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all 
#     weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an 
#     example of what this plot should look like using simulated data.

# First we need to calculate the average steps per interval for each weekdays or weekends.

AvgStepsWkInterval <- activity_clean %>% group_by(interval, Day) %>% summarise(steps = mean(steps))

# Next, we need to create plots for each weekday (plotWkdy) and weekend(plotwknd)

ggplot(AvgStepsWkInterval)+ 
   geom_line(aes(interval, steps, color = Day), size = 1)+
   labs(x= "Day Interval (5min)", y= "Average Steps Taken", title= "Average Steps: Weekday vs. Weekend") +
   facet_grid(rows = vars(Day)) +
   theme(legend.position="none")
   
