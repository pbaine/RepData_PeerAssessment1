Loading and preprocessing the data
----------------------------------

Show any code that is needed to

1.  Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2.  Process/transform the data (if necessary) into a format suitable for
    your analysis

<!-- -->

    library(dplyr, quietly = T)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2, quietly = T)

    import <- read.csv("activity.csv")
    activity <- import
    activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
    activity$steps[is.na(activity$steps)] <- 0

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in
the dataset.

1.  Calculate the total number of steps taken per day
2.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day
3.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    steps <- activity %>% group_by(date) %>% summarize(steps = sum(steps))
    ggplot(steps, aes(x = steps)) + geom_histogram(binwidth = 1000, boundary = 0) +
          theme_bw() + xlab("Number of steps per day") + ylab("Count") +
          ggtitle("Total number of steps taken each day") + 
          scale_y_continuous(breaks = c(0,2,4,6,8,10))

![](PA1_Template_files/figure-markdown_strict/Steps%20Histogram-1.png)

    mean <- mean(steps$steps)
    median <- median(steps$steps)

The mean number of steps per day is 9354.2295082 and the median number
of steps per day is 1.039510^{4}.

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)
2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    intervalmean <- activity %>% group_by(interval) %>% summarize(mean = mean(steps))
    ggplot(intervalmean, aes(x = interval, y = mean)) + geom_line(color = "firebrick") +
          ylab("Average steps per interval") + xlab("Interval") + 
          ggtitle("Average number of steps taken per interval")

![](PA1_Template_files/figure-markdown_strict/Time%20series-1.png)

    maxval <- max(intervalmean$mean)
    maxinter <- intervalmean$interval[intervalmean$mean == maxval]

The interval with the largest mean is 835 with a mean of 179.1311475.

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values (coded as 𝙽𝙰). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with 𝙽𝙰s)
2.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.
3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.
4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    natotals <- sum(is.na(import$steps))

There are 2304 NA values in the steps data.

    fill <- import
    for(j in 1:nrow(fill)){
          if(is.na(fill$steps[j])) {
                fill$steps[j] <- intervalmean$mean[intervalmean$interval == fill$interval[j]]    
          }
    }

    fillsum <- fill %>% group_by(date) %>% summarize(steps = sum(steps))
    fillmean <- mean(fillsum$steps)
    fillmedian <- median(fillsum$steps)

    ggplot(fillsum, aes(x = steps)) + geom_histogram(binwidth = 1000, boundary = 0) +
          theme_bw() + xlab("Number of steps per day") + ylab("Count") +
          ggtitle("Total number of steps taken each day") + scale_y_continuous(breaks =c(0,2,4,6,8,10))

![](PA1_Template_files/figure-markdown_strict/Filling%20in%20Missing%20Data-1.png)

The mean number of steps per day is 1.058101410^{4} and the median
number of steps per day is 1.039510^{4}. The mean rose from the unfilled
data, which had a mean of 9354.2295082, but the median stayed the same.

Inputting missing data would make the mean rise, because previously the
numbers were being counted as zeros. The median would remain in the same
place.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.
2.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    fill$date <- as.Date(fill$date, format = "%Y-%m-%d")
    fill$day <- weekdays(fill$date)
    fill$cat <- ifelse(fill$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
    intervalfacet <- fill %>% group_by(interval, cat) %>% summarize(mean = mean(steps))
    ggplot(intervalfacet, aes(x = interval, y = mean)) + geom_line(color = "firebrick") +
          facet_grid(cat~.) + ylab("Average steps per interval") +
          xlab("Interval") + ggtitle("Average number of steps taken per interval")

![](PA1_Template_files/figure-markdown_strict/Weekdays%20and%20Weekends-1.png)
