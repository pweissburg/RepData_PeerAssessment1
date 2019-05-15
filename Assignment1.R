install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(lubridate)
# 1. Load data into R
# Create temp file in WD
temp <- tempfile()
# Download file from URL to temp file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
# Load data into R
data <- as.tbl(read.csv(unz(temp, "activity.csv")))
# Remove temp file
unlink(temp)

# 2. Histogram of # of steps taken each day
steps1 <- data %>%
          group_by(date) %>%
          summarise(sum(steps,na.rm = TRUE))
names(steps1) <- c("date","steps")
qplot(steps1$steps,bins = 50)

# 3. Median and mean of step taken each day
summary(steps1$steps)
#median = 10395; mean = 9354

# 4. Time series plot of number of steps taken
ggplot(data, aes(interval,steps))+geom_point()

# 5. The 5-minute interval that, on average,
#    contains the maximum number of steps
filter(data, data$steps == max(data$steps,na.rm = TRUE))
# Interval number 615 has the most steps (806 steps)

# 6. Imputing Missing Values
# 6a. Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))
# 6b. Devise a strategy to impute NA values
# Use median for each interval
# 6c. Create imputed data sets
dataNew <- data %>%
  group_by(interval) %>%
  mutate(median = median(steps,na.rm = TRUE))%>%
  mutate(steps2 = case_when(is.na(steps) ~ median,!is.na(steps) ~ steps))%>%
  mutate(day = weekdays(as.Date(date)))%>%
  mutate(weekend = case_when(day == "Saturday" | day == "Sunday" ~ "Weekend",
                             TRUE ~ "Weekday"))
dataNew$weekend <- factor(dataNew$weekend,levels = c("Weekday","Weekend"))

dataWeekend <- group_by(dataNew,interval,weekend)%>%
  summarise(mean = mean(steps2))

dataNew <- group_by(dataNew,date)%>%
  summarise(sum = sum(steps2))

# 6d. Make a histogram with imputed data
qplot(dataNew$sum,bins = 30)
# 6e. Find median and mean for imputed data
median(dataNew$sum) #median = 10395
mean(dataNew$sum) #mean = 9503.869
# 6f. The median is the same as the non-imputed calculation but
# the mean is lower.

# 7. What is the average daily activity pattern?
#  Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
#  of the 5-minute interval (x-axis) and the average number of steps taken,
#  averaged across all days (y-axis)
gg <- ggplot(dataWeekend,aes(interval,mean))
gg + geom_line()+facet_grid(weekend~.)

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
dataMean <- group_by(dataNew,interval)%>%
  summarise(mean = mean(steps2))
dataMean[dataMean$mean == max(dataMean$mean),]
# Interval 835 with mean 182 ***Calculated with imputed data***
