-   library(dplyr)
-   library(tidyverse)
-   library(lubridate)

### 1. Load data into R

Create temp file in WD

    temp <- tempfile()

Download file from URL to temp file

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)

Load data into R

    data <- as.tbl(read.csv(unz(temp, "activity.csv")))

Remove temp file

    unlink(temp)

### 2. Histogram of \# of steps taken each day

    steps1 <- data %>%
              group_by(date) %>%
              summarise(sum(steps,na.rm = TRUE))
    names(steps1) <- c("date","steps")
    qplot(steps1$steps,bins=50)

![](Assign1_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### 3. Median and mean of steps taken each day

    summary(steps1$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

median = 10395; mean = 9354

### 4. Time series plot of number of steps taken

    dataNArm <- data[!is.na(data$steps),]
    ggplot(dataNArm, aes(interval,steps))+geom_point()

![](Assign1_files/figure-markdown_strict/unnamed-chunk-8-1.png)

### 5. The 5-minute interval that, on average, contains the maximum number of steps

    filter(data, data$steps == max(data$steps,na.rm = TRUE))

    ## # A tibble: 1 x 3
    ##   steps date       interval
    ##   <int> <fct>         <int>
    ## 1   806 2012-11-27      615

Interval number 615 has the most steps (806 steps)

### 6. Imputing Missing Values

6a. Calculate and report the total number of missing values in the
dataset

    sum(is.na(data$steps))

    ## [1] 2304

6b. Devise a strategy to impute NA values: Use median for each
interval  
6c. Create imputed data sets

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

    dataNew2 <- group_by(dataNew,date)%>%
      summarise(sum = sum(steps2))

6d. Make a histogram with imputed data

    qplot(dataNew2$sum,bins = 30)

![](Assign1_files/figure-markdown_strict/unnamed-chunk-12-1.png)

6e. Find median and mean for imputed data

    summary(dataNew2$sum)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    6778   10395    9504   12811   21194

median = 10395 mean = 9503.869

6f. The median is the same as the non-imputed calculation but the mean
is higher.

### 7. What is the average daily activity pattern?

Make a time series plot (i.e. type="l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    gg <- ggplot(dataWeekend,aes(interval,mean))
    gg + geom_line()+facet_grid(weekend~.)

![](Assign1_files/figure-markdown_strict/unnamed-chunk-14-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    dataMean <- group_by(dataNew,interval)%>%
      summarise(mean = mean(steps2))
    dataMean[dataMean$mean == max(dataMean$mean),]

    ## # A tibble: 1 x 2
    ##   interval  mean
    ##      <int> <dbl>
    ## 1      835  182.

Interval 835 with mean 182 ***Calculated with imputed data***
