Reproducible Research
=====================
###Peer Assesment I

##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

These are the R libraries that will be used:


```r
library(ggplot2)
library(plyr)
library(knitr)
library(xtable)
```

##Data

## Loading and preprocessing the data

The data for this assignment can be downloaded from the course web site:
  

```r
if(!file.exists("./RR_Project1")){dir.create("./RR_Project1")}
setwd("./RR_Project1")
fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="Project1_DATA.zip")
unzip("Project1_DATA.zip")
```

Dataset: Activity monitoring data [52K]


```r
DataFrame <- read.csv("activity.csv",
    colClasses = c("integer", "Date", "integer"),
  	na.strings = "NA")

str(DataFrame)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken


```r
xt <- xtable(summary(DataFrame))
print(xt, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Mon Aug 18 01:00:31 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH>     steps </TH> <TH>      date </TH> <TH>    interval </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Min.   :  0.0   </TD> <TD> Min.   :2012-10-01   </TD> <TD> Min.   :   0   </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> 1st Qu.:  0.0   </TD> <TD> 1st Qu.:2012-10-16   </TD> <TD> 1st Qu.: 589   </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Median :  0.0   </TD> <TD> Median :2012-10-31   </TD> <TD> Median :1178   </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Mean   : 37.4   </TD> <TD> Mean   :2012-10-31   </TD> <TD> Mean   :1178   </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> 3rd Qu.: 12.0   </TD> <TD> 3rd Qu.:2012-11-15   </TD> <TD> 3rd Qu.:1766   </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> Max.   :806.0   </TD> <TD> Max.   :2012-11-30   </TD> <TD> Max.   :2355   </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> NA's   :2304   </TD> <TD>  </TD> <TD>  </TD> </TR>
   </TABLE>

##Assignment

###What is mean total number of steps taken per day?

For this part of the assignment, we can ignore the missing values in the dataset.


```r
DF <- DataFrame[complete.cases(DataFrame),]
```

Let's calculate first the total number of steps taken each day


```r
SumStepsByDay <- data.frame(  
    Date = unique(DF$date), 
    AverageSteps = as.numeric(tapply(DF$step, DF$date, sum)))

Stats1 <- data.frame(
    Stats = c("Mean w/o NAs", "Median w/o NAs", "Variance w/o NAs"), 
    Values = c(mean(SumStepsByDay$AverageSteps),median(SumStepsByDay$AverageSteps),var(SumStepsByDay$AverageSteps)))
```
The mean of total number of steps is: 1.0766 &times; 10<sup>4</sup>, the median: 1.0765 &times; 10<sup>4</sup>


```r
Table1 <- xtable(Stats1)
print(Table1, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Mon Aug 18 01:00:31 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Stats </TH> <TH> Values </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Mean w/o NAs </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Median w/o NAs </TD> <TD align="right"> 10765.00 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Variance w/o NAs </TD> <TD align="right"> 18225902.08 </TD> </TR>
   </TABLE>
# 
Below the code to print the barchart of total number of steps taken each day. although this is requested by the assignment, I think it helps to understand better the data structure. The dotted line is the mean of total number of steps taken per day


```r
  	ggplot(data = SumStepsByDay) +
			 geom_bar(	aes(x = Date, y = AverageSteps),
					stat = "identity", 
					fill =  "steelblue",
					alpha = 0.6) +
			 geom_line(	aes(x = Date, y = mean(AverageSteps)),
					linetype = "dashed", 
					size = 0.8,
					colour = "black", alpha = 0.4) + 
  		 	 xlab("Days") +
			 ylab("Total number of steps") +
  		  	 ggtitle("Total number of steps taken each day") + 
			 theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A01-04](./PA1_template_files/figure-html/A01-04.png) 

Histogram of the total number of steps taken each day. The dotted line is the mean of total number of steps taken per day


```r
  	ggplot(data = SumStepsByDay) +
			 geom_histogram(	aes(x = AverageSteps),
						binwidth = max(SumStepsByDay$AverageSteps)/9, 
						colour = "gray90", 
						fill =  "steelblue",
						alpha = 0.6) +
   			 geom_vline(	aes(xintercept = mean(AverageSteps)),
						linetype = "dashed", 
						size = 0.8,
						colour = "black", alpha = 0.4) + 
  		 	 xlab("Total number of steps") +
			 ylab("Frequency (Nr. of days)") +
  		  	 ggtitle("Histogram of total number of Steps per day (w/o NAs)") + 
			 theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A01-05](./PA1_template_files/figure-html/A01-05.png) 

### What is the average daily activity pattern?

Let's calculate average number of steps taken, averaged across all days 


```r
AvgStepsByInterval <- data.frame(
    Interval = unique(DF$interval), 
    AverageSteps = as.numeric(tapply(DF$step, DF$interval, mean)))

MaxStepsByInterval <- AvgStepsByInterval[AvgStepsByInterval$AverageSteps == max(AvgStepsByInterval$AverageSteps),]	

print(MaxStepsByInterval)
```

```
##     Interval AverageSteps
## 104      835        206.2
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835 (time interval corresponds to 13:55)

Below a time series plot (using ggplot2) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). The dotted line highlights the 5 minutes interval with the maximum average number of steps. 


```r
  	ggplot(data = AvgStepsByInterval) + 
			geom_line( 	aes(x = Interval, y = AverageSteps),
					linetype = "solid", 
					size = 1.2,
					colour = "steelblue", alpha = 0.7) + 
			geom_area( 	aes(x = Interval, y = AverageSteps),
					fill = "steelblue", alpha = 0.2) + 
   			geom_vline(	aes(xintercept = MaxStepsByInterval[[1]]),
					linetype = "dashed", 
					size = 0.8,
					colour = "black", alpha = 0.4) + 
  		 	xlab("5-minute intervals") +
			ylab("Average Nr. of steps (across all days)") +
  		  	ggtitle("Average Nr. of steps per 5-minute interval") + 
			theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A02-02](./PA1_template_files/figure-html/A02-02.png) 

### Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
str(DataFrame)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
sum(is.na(DataFrame)[,1]); mean(is.na(DataFrame)[,1])
```

```
## [1] 2304
```

```
## [1] 0.1311
```

The total number of NAs is 2304 that corresponds to the 13.1148% of the total number of observations

Let's devise a strategy for filling in all of the missing values in the dataset. 

First let's create a database with the missing values:


```r
Nas <- DataFrame[!complete.cases(DataFrame),]
tapply(Nas$interval, Nas$date, length)
```

```
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

```r
NasDates <- unique(Nas$date); length(NasDates)
```

```
## [1] 8
```
We can see that the values missing are the values of 8 missing full days:


```r
print(NasDates)
```

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```
I will fill the interval missing values with the mean of the 5-minute intervals averaged across all days.


```r
FilledSteps <- round(rep(AvgStepsByInterval$AverageSteps,length(NasDates)))
```
Now, the two dataset are merged to create a new dataset that is equal to the original dataset but with the missing data filled in. A new factor variable "type" is added to the dataset tho flag the data as "Actual" or "Filled"


```r
DFActual <- DF
    DFActual$type <- as.factor("Actual")

DFFilled <- Nas 
    DFFilled$steps <- FilledSteps
    DFFilled$type <- as.factor("Filled")
		
DataFrameFilled <- rbind(DFActual, DFFilled)
    DataFrameFilled <- DataFrameFilled[order(DataFrameFilled$date, DataFrameFilled$interval),]
		
str(DataFrameFilled)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  2 0 0 0 0 2 1 1 0 1 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ type    : Factor w/ 2 levels "Actual","Filled": 2 2 2 2 2 2 2 2 2 2 ...
```

Let's calculate the average number of steps taken, averaged across all days.


```r
SumFilledStepsByDay <- data.frame(  Date = unique(DataFrameFilled$date), 
    AverageSteps = as.numeric(tapply(DataFrameFilled$step, DataFrameFilled$date, sum)),
    Type = as.factor(as.character(tapply(DataFrameFilled$type, DataFrameFilled$date, function(get){as.character(get[1])}))))
		
Stats2 <- data.frame(
    Stats = c("Mean Filled", "Median Filled", "Variance Filled"), 
    Values = c(mean(SumFilledStepsByDay$AverageSteps),median(SumFilledStepsByDay$AverageSteps),var(SumFilledStepsByDay$AverageSteps)))
```
The mean of total number of steps is: 1.0766 &times; 10<sup>4</sup>, the median: 1.0765 &times; 10<sup>4</sup>


```r
Table2 <- xtable(Stats2)
print(Table2, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Mon Aug 18 01:00:33 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Stats </TH> <TH> Values </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Mean Filled </TD> <TD align="right"> 10765.64 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Median Filled </TD> <TD align="right"> 10762.00 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Variance Filled </TD> <TD align="right"> 15795783.83 </TD> </TR>
   </TABLE>
# 
Below the code to print the barchart of total number of steps taken each day. although this is requested by the assignment, I think it helps to understand better the data structure. The dotted line is the mean of total number of steps taken per day


```r
  	ggplot(data = SumFilledStepsByDay) +
			 geom_bar(	aes(x = Date, y = AverageSteps, fill = Type),
					stat = "identity", 
					alpha = 0.6) +
			 geom_line(	aes(x = Date, y = mean(AverageSteps)),
					linetype = "dashed", 
					size = 0.8,
					colour = "black", alpha = 0.4) + 
			 scale_fill_manual(values = c("steelblue","tomato2")) +
  		 	 xlab("Days") +
			 ylab("Total number of steps") +
  		  	 ggtitle("Total number of steps taken each day") + 
			 theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A03-08](./PA1_template_files/figure-html/A03-08.png) 

Histogram of the total number of steps taken each day. The dotted line is the mean of total number of steps taken per day


```r
  	ggplot(data = SumFilledStepsByDay) +
			 geom_histogram(	aes(x = AverageSteps),
						binwidth = max(SumFilledStepsByDay$AverageSteps)/9, 
						colour = "gray90", 
						fill =  "violetred4",
						alpha = 0.6) +
   			 geom_vline(	aes(xintercept = mean(AverageSteps)),
						linetype = "dashed", 
						size = 0.8,
						colour = "black", alpha = 0.4) + 
  		 	 xlab("Total number of steps") +
			 ylab("Frequency (Nr. of days)") +
  		  	 ggtitle("Histogram of total number of Steps per day (filled)") + 
			 theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A03-09](./PA1_template_files/figure-html/A03-09.png) 

The chart below compares the Histograms of filled DataFrame (purple) and Dataframe w/o Nas (blue). The two charts are overlapped. 


```r
  	ggplot() +
			 geom_histogram(	data = SumFilledStepsByDay,
						aes(x = AverageSteps),
						binwidth = max(SumFilledStepsByDay$AverageSteps)/9, 
						colour = "gray90", 
						fill =  "violetred4",
						alpha = 0.6) +
			 geom_histogram(	data = SumStepsByDay,
						aes(x = AverageSteps),
						binwidth = max(SumStepsByDay$AverageSteps)/9, 
						colour = "gray90", 
						fill =  "steelblue",
						alpha = 0.6) +
   			 geom_vline(	data = SumStepsByDay,
						aes(xintercept = mean(AverageSteps)),
						linetype = "dotted", 
						size = 0.8,
						colour = "black", alpha = 0.4) + 
   			 geom_vline(	data = SumFilledStepsByDay,
						aes(xintercept = mean(AverageSteps)),
						linetype = "dashed", 
						size = 0.8,
						colour = "black", alpha = 0.4) + 
  		 	 xlab("Total number of steps") +
			 ylab("Frequency (Nr. of days)") +
  		  	 ggtitle("Histograms of total number of Steps (filled and w/o NAs)") + 
			 theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A03-10](./PA1_template_files/figure-html/A03-10.png) 

From the chart above we can see that the histogram of the filled database (Purple) shows more density around the mean. The purple histogram shows more frequency in the range that includes the mean and shows less dispersion. This was expected as the filled values are the averages of the 5-minute intervals averaged across all days. The estimates of the total daily number of steps will be closer to the average value of actual data.

below the statistics compared:


```r
Stats3 <- rbind(Stats1,Stats2)
Table3 <- xtable(Stats3)
print(Table3, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Mon Aug 18 01:00:34 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Stats </TH> <TH> Values </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Mean w/o NAs </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Median w/o NAs </TD> <TD align="right"> 10765.00 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Variance w/o NAs </TD> <TD align="right"> 18225902.08 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Mean Filled </TD> <TD align="right"> 10765.64 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Median Filled </TD> <TD align="right"> 10762.00 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> Variance Filled </TD> <TD align="right"> 15795783.83 </TD> </TR>
   </TABLE>
###Are there differences in activity patterns between weekdays and weekends?

Creates a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Days <- weekdays(DataFrameFilled$date)
    Days[which(Days == "Saturday" | Days == "Sunday")] <- "Weekend"
    Days[which(Days != "Weekend")] <- "Weekday"

DataFrameFilled$day <- as.factor(Days)

AvgFilledStepsByInterval <- ddply(DataFrameFilled, c("interval","day"), function(df)mean(df$steps))
names(AvgFilledStepsByInterval) <- c("Interval", "Day", "AverageSteps")

summary(AvgFilledStepsByInterval, digits = 7)
```

```
##     Interval           Day       AverageSteps   
##  Min.   :   0.0   Weekday:288   Min.   :  0.00  
##  1st Qu.: 588.8   Weekend:288   1st Qu.:  2.11  
##  Median :1177.5                 Median : 28.12  
##  Mean   :1177.5                 Mean   : 38.99  
##  3rd Qu.:1766.2                 3rd Qu.: 61.23  
##  Max.   :2355.0                 Max.   :230.36
```
Makes a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
  	ggplot(data = AvgFilledStepsByInterval) + 
			geom_line( 	aes(x = Interval, y = AverageSteps),
					linetype = "solid", 
					size = 1,
					colour = "violetred4", alpha = 0.7) + 
			facet_wrap( ~ Day, nrow=2) +
			geom_area( 	aes(x = Interval, y = AverageSteps),
					fill = "violetred4", alpha = 0.1) + 
  		 	xlab("5-minute intervals") +
			ylab("Average Nr. of steps (across all days)") +
  		  	ggtitle("Average Nr. of steps per 5-minute interval") + 
			theme(plot.title = element_text(face = "bold"))
```

![plot of chunk A04-02](./PA1_template_files/figure-html/A04-02.png) 

