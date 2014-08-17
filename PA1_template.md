# Reproducible Research: Peer Assessment 1

Sport Activity Analysis With Knitr
==================================

## Loading and preprocessing the data

Since the data has to be loaded from a zipped file, 
one would like to use unz().
This way the  html file can be attained by only running 
library(knitr) and knit2html("PA1_template.Rmd") from R.


```r
zipHandle<-unz("activity.zip", "activity.csv")
rawData<-read.table(zipHandle, header=TRUE, sep=",")
unlink(zipHandle)
rm(zipHandle)
```

As preprocessing the second column will be converted in a POSIXlt type.
Two other columns are also made in the right type 
and a dataframe is made from this.


```r
date<-as.POSIXct(rawData$date)
steps<-as.integer(rawData$steps)
interval<-as.character(rawData$interval)
df <- data.frame(date, interval, steps)
rm(rawData)
rm(date)
rm(interval)
rm(steps)
```

## What is mean total number of steps taken per day?
The dataframe from the file (called df) has one column with dates.
From this column a list of all the days the device was creating
data can be compiled.
Also the number of days the device was making data is determined.


```r
# first find out how many days the sensor was running
startDate<-df$date[1]
endDate<-df$date[length(df$date)]

# make a list containing the days on which the device was on
listOfDays = unique(df$date)

# calculate the number of days the device was on
numberOfDays = as.integer(length(listOfDays))
```

A for loop is used to evaluate for every day three required statistics, which are
* The mean of all the steps for one day.
* The sum of all the steps for one day.
* The median of all the steps for one day.


```r
# create a list on which we will append a mean for every
# run trough the for-loop
listOfMeans = as.numeric(matrix(NA, numberOfDays, 1))
listOfSums = as.numeric(matrix(NA, numberOfDays, 1))
listOfMedians = as.numeric(matrix(NA,numberOfDays,1))

# walk trough all the days and calculate properties for it
for ( daySelect in 1:numberOfDays)
{

    # subset only one day of the set
    dayData = subset(df, date == listOfDays[daySelect])

    # calculate the mean and sum for one day
    meanOfOneDay = mean(dayData$steps, na.rm = TRUE)
    sumOfOneDay = sum(dayData$steps, na.rm = TRUE)
    medianOfOneDay = median(dayData$steps, na.rm = TRUE)

    # write the new mean and sum in the list
    listOfMeans[[daySelect]] = meanOfOneDay
    listOfSums[[daySelect]] = sumOfOneDay
    listOfMedians[[daySelect]] = medianOfOneDay

}
```
For the report part, it is important to see that the interval is actually
registered every 5 minutes, thus the dimension of the mean table is in 
"average number of steps per 5 minutes", which is also the case for the median values.
The histogram is expressed in the total number of steps per day.

```r
# plot the histogram for the total number of steps
hLabel = "Histogram of the total number of steps per day"
hist(listOfSums, breaks = 10, xlab = "steps taken per day", main = hLabel)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
# report the mean and median for the total steps taken every day
print(listOfMeans)
```

```
##  [1]     NaN  0.4375 39.4167 42.0694 46.1597 53.5417 38.2465     NaN
##  [9] 44.4826 34.3750 35.7778 60.3542 43.1458 52.4236 35.2049 52.3750
## [17] 46.7083 34.9167 41.0729 36.0938 30.6285 46.7361 30.9653 29.0104
## [25]  8.6528 23.5347 35.1354 39.7847 17.4236 34.0938 53.5208     NaN
## [33] 36.8056 36.7049     NaN 36.2465 28.9375 44.7326 11.1771     NaN
## [41]     NaN 43.7778 37.3785 25.4722     NaN  0.1424 18.8924 49.7882
## [49] 52.4653 30.6979 15.5278 44.3993 70.9271 73.5903 50.2708 41.0903
## [57] 38.7569 47.3819 35.3576 24.4688     NaN
```

```r
print(listOfMedians)
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```

## What is the average daily activity pattern?

Calculating the average daily activity is essentially done in the same way as
calculating the mean for every day.
This time we iterate over all time intervals, and for every time interval we
will have to go along all days to fetch the number of steps.
This way we will find an array 


```r
# Make a list of all possible intervals, which are 12 per hour times 24 per day
listOfIntervals = unique(df$interval)
# Calculate the number of bins
numberOfIntervals = length(listOfIntervals)

# Array in which one element contains the number of steps for a specificInterval 
# Every index is one day.
listOfMeansInterval = as.numeric(matrix(NA, numberOfDays, 1))

# walk along all intervals
for ( intervalSelect in 1:numberOfIntervals )
{

   # subset only one time interval of the set
   extractedTimeInterval = as.character(listOfIntervals[intervalSelect])
   intervalData = subset(df, df$interval == extractedTimeInterval)

   # calculate the mean and put it in the list of intervalMeans
   meanOfOneInterval = mean(intervalData$steps, na.rm = TRUE)

   # write the new mean in the list
   listOfMeansInterval[[intervalSelect]] = meanOfOneInterval

}

# Now we are actually done, however I want to find every interval for every
# selected interval

xplot = as.numeric(matrix(NA, numberOfIntervals, 1))
yplot = as.numeric(matrix(NA, numberOfIntervals, 1))

for ( intervalSelect in 1:numberOfIntervals)
{
    xplot[[intervalSelect]] = as.character(listOfIntervals[intervalSelect])
    yplot[[intervalSelect]] = listOfMeansInterval[[intervalSelect]]
}

# So now plot the figure
plot(xplot, yplot, xlab = "Time on day (interval)", ylab = "Steps per 5 minutes" ,type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The report part should find the peak in this graph, which is around 835, 
so maybe the person likes to walk every morning around 8:35.
Also he seems to never do any sport before 5 AM in the morning and after 20:00
in the evening.

## Imputing missing values

The total number of rows in the dataset with "NA" can be found by using sum()

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```
Which thus is just a small part, since the whole dataset has 17568 rows.

I would propose to fill all "NA"'s of one interval with the mean of that interval,
since some days only have "NA" values, in which case it is not possible to
calculate a mean.

I will walk trough the df$steps list, and if I find a NA value, I will
seek the time interval of that row.
From there I will determine which mean in the "listOfMeansInterval" 
corresponds to this interval and overwrite the "NA" value.

After that I will just plot the dataframe in the same way as in part one of the assignment.


```r
# find the number of rows in the dataset
numberOfRows = length(df$steps)

# walk along every row
for ( rowSelect in 1:numberOfRows )
{
    if ( is.na(df$steps[rowSelect]) )
    {

        # This block is executed if an NA is encountered

        # One needs the interval
        intervalIndex = as.character(df$interval[rowSelect])
        index = as.integer(subset(listOfIntervals, listOfIntervals == intervalIndex))

        # Overwrite the NA in the original array
        df$steps[rowSelect] = listOfMeansInterval[index]
    }

}
```
From here I can do exactly the same as in assignment 2 in which I had to
plot a histogram of the sum of all steps taken in one day, and report the mean and median.

```r
# repeat the important piece of code to calculate the mean and median
# for every day

# walk trough all the days and calculate properties for it
for ( daySelect in 1:numberOfDays)
{

    # subset only one day of the set
    dayData = subset(df, date == listOfDays[daySelect])

    # calculate the mean and sum for one day
    meanOfOneDay = mean(dayData$steps, na.rm = TRUE)
    sumOfOneDay = sum(dayData$steps, na.rm = TRUE)
    medianOfOneDay = median(dayData$steps, na.rm = TRUE)

    # write the new mean and sum in the list
    listOfMeans[[daySelect]] = meanOfOneDay
    listOfSums[[daySelect]] = sumOfOneDay
    listOfMedians[[daySelect]] = medianOfOneDay

}

# plot the histogram for the total number of steps
hLabel = "Histogram of the total number of steps per day (NA's replaced)"
hist(listOfSums, breaks = 10, xlab = "steps taken per day", main = hLabel)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
# report the mean and median for the total steps taken every day
print(listOfMeans)
```

```
##  [1] 37.3826  0.4375 39.4167 42.0694 46.1597 53.5417 38.2465 37.3826
##  [9] 44.4826 34.3750 35.7778 60.3542 43.1458 52.4236 35.2049 52.3750
## [17] 46.7083 34.9167 41.0729 36.0938 30.6285 46.7361 30.9653 29.0104
## [25]  8.6528 23.5347 35.1354 39.7847 17.4236 34.0938 53.5208 37.3826
## [33] 36.8056 36.7049 37.3826 36.2465 28.9375 44.7326 11.1771 37.3826
## [41] 37.3826 43.7778 37.3785 25.4722 37.3826  0.1424 18.8924 49.7882
## [49] 52.4653 30.6979 15.5278 44.3993 70.9271 73.5903 50.2708 41.0903
## [57] 38.7569 47.3819 35.3576 24.4688 37.3826
```

```r
print(listOfMedians)
```

```
##  [1] 34.11  0.00  0.00  0.00  0.00  0.00  0.00 34.11  0.00  0.00  0.00
## [12]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
## [23]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00 34.11  0.00
## [34]  0.00 34.11  0.00  0.00  0.00  0.00 34.11 34.11  0.00  0.00  0.00
## [45] 34.11  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
## [56]  0.00  0.00  0.00  0.00  0.00 34.11
```
The means do not have NaN values anymore which occured since for some
days all values where NA.
The NA values in the medians are all replaced by the same number, 34.11.
The histogram has less values in the highest and the lowest regions, 
more values are close the the mean.

## Are there differences in activity patterns between weekdays and weekends?


This time I will do exactly the same as in part 3 but now I will subset the
interval and also the weekday.


```r
isWeekend = factor(matrix(NA, numberOfIntervals, 1), level = c("weekday", "weekend"))
wdf<-cbind(df, isWeekend)


for ( rowSelect in 1:numberOfRows)
{

    weekDay = weekdays(df$date[rowSelect])
    if (  weekDay ==  "zondag" | weekDay == "zaterdag" ) {
        wdf$isWeekend[rowSelect] = "weekend"
    }
    else
    {
        wdf$isWeekend[rowSelect] = "weekday"
    }

}

listOfMeansIntervalWeekday = as.numeric(matrix(NA, numberOfDays, 1))
listOfMeansIntervalWeekend = as.numeric(matrix(NA, numberOfDays, 1))

for (intervalSelect in 1:numberOfIntervals)
{
    
    extractedTimeInterval = as.character(listOfIntervals[intervalSelect])
    intervalDataWeekday = subset(wdf, wdf$interval == extractedTimeInterval & wdf$isWeekend =="weekday")
    intervalDataWeekend = subset(wdf, wdf$interval == extractedTimeInterval & wdf$isWeekend == "weekend")
    
    meanOfOneIntervalWeekday = mean(intervalDataWeekday$steps, na.rm = TRUE)
    meanOfOneIntervalWeekend = mean(intervalDataWeekend$steps, na.rm = TRUE)
    
    listOfMeansIntervalWeekday[[intervalSelect]] = meanOfOneIntervalWeekday
    listOfMeansIntervalWeekend[[intervalSelect]] = meanOfOneIntervalWeekend
    
}

xplotWeekend = as.numeric(matrix(NA, numberOfIntervals, 1))
yplotWeekend = as.numeric(matrix(NA, numberOfIntervals, 1))

for ( intervalSelect in 1:numberOfIntervals)
{
    xplotWeekend[[intervalSelect]] = as.character(listOfIntervals[intervalSelect])
    yplotWeekend[[intervalSelect]] = listOfMeansIntervalWeekend[[intervalSelect]]
}

# plot(xplotWeekend, yplotWeekend, xlab = "Time on day (interval)", ylab = "Steps per 5 minutes" ,type = "l")

xplotWeekday = as.numeric(matrix(NA, numberOfIntervals, 1))
yplotWeekday = as.numeric(matrix(NA, numberOfIntervals, 1))

for ( intervalSelect in 1:numberOfIntervals)
{
    xplotWeekday[[intervalSelect]] = as.character(listOfIntervals[intervalSelect])
    yplotWeekday[[intervalSelect]] = listOfMeansIntervalWeekday[[intervalSelect]]
}

# plot(xplotWeekday, yplotWeekday, xlab = "Time on day (interval)", ylab = "Steps per 5 minutes" ,type = "l")

par(mfrow=c(2,1))
plot(xplotWeekend, yplotWeekend, xlab = "Time on day (interval)", ylab = "Steps per 5 minutes" ,type = "l", main = "Mean of number of steps in a weekend")
plot(xplotWeekday, yplotWeekday, xlab = "Time on day (interval)", ylab = "Steps per 5 minutes" ,type = "l", main = "Mean of number of steps on a weekday")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

It is clear that between 1000 and 1600 the person had less chance to excercise during the weekdays
and more time for this in the weekend.
