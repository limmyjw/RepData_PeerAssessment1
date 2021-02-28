---
title: "Assignment_Week2"
author: "Jan Willem"
date: "28-2-2021"
output: 
  html_document:
    keep_md: true
---
  
  
  ## Assignment
  
  1 Code for reading in the dataset and/or processing the data  
2 Histogram of the total number of steps taken each day  
3 Mean and median number of steps taken each day  
4 Time series plot of the average number of steps taken  
5 The 5-minute interval that, on average, contains the maximum number of steps  
6 Code to describe and show a strategy for imputing missing data  
7 Histogram of the total number of steps taken each day after missing values are imputed  
8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends  
9 All of the R code needed to reproduce the results (numbers, plots, etc.) in the report  


### 1 Code for reading in the dataset and/or processing the data

Download the data


```r
options(scipen = 999) 
#Download and unzip files
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
              , destfile = paste(getwd(), "repdata_data_activity.zip", sep = "/"))
#unzip(zipfile = "repdata_data_activity.zip")
unzip(zipfile =  paste(getwd(), "repdata_data_activity.zip", sep = "/"))
```
Check for packages that are needed 

```
## [[1]]
## [1] "dplyr"     "stats"     "graphics"  "grDevices" "utils"     "datasets" 
## [7] "methods"   "base"     
## 
## [[2]]
## [1] "ggplot2"   "dplyr"     "stats"     "graphics"  "grDevices" "utils"    
## [7] "datasets"  "methods"   "base"     
## 
## [[3]]
##  [1] "gridExtra" "ggplot2"   "dplyr"     "stats"     "graphics"  "grDevices"
##  [7] "utils"     "datasets"  "methods"   "base"     
## 
## [[4]]
##  [1] "imputeTS"  "gridExtra" "ggplot2"   "dplyr"     "stats"     "graphics" 
##  [7] "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[5]]
##  [1] "lubridate" "imputeTS"  "gridExtra" "ggplot2"   "dplyr"     "stats"    
##  [7] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"
```


Read file and look at the head of the file

```r
#paste(getwd(), , sep = "/"))
activity<-read.csv(file=paste(getwd(), "activity.csv" , sep = "/"), header=TRUE, sep=",")
#activity<-read.csv(file="activity.csv" , header=TRUE, sep=",")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Change datatype date to date()

```r
activity$date<-as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
### 2 Histogram of the total number of steps taken each day


```r
steps_each_day<-activity%>%
  group_by(date)%>%
  summarise_at(vars(steps),list(sum=sum))
ggplot(data=steps_each_day, aes(sum)) + 
  geom_histogram(binwidth = 2000)+ 
  labs(title="Total number of steps taken each day") +
  labs(x="Total steps",y="Freqency")
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#png(filename="plot1.png")
#plot(g1)
#dev.off()
```

### 3 Mean and median number of steps taken each day


```r
mediaan<-median(steps_each_day$sum, na.rm=TRUE)
gemiddeld<-round(mean(steps_each_day$sum, na.rm=TRUE),2)
mediaan
```

```
## [1] 10765
```

```r
gemiddeld
```

```
## [1] 10766.19
```


The median of the number of steps taken each day is:  10765.
The mean of the number of steps is: 10766.19

### 4 Time series plot of the average number of steps taken each interval



```r
steps_each_day_interval<-activity%>%
  group_by(interval)%>%
  summarise_at(vars(steps),list(mean=mean),na.rm=TRUE)
ggplot(steps_each_day_interval, aes(interval, mean)) + geom_line() 
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 5 The 5-minute interval that, on average, contains the maximum number of steps


```r
avg_most_steps<-activity%>%                            # data frame
  group_by(interval) %>%                 # group indicator
  summarise_at(vars(steps),              # column
               list(gemiddeld = mean), na.rm=TRUE)
moststeps<-subset(avg_most_steps,gemiddeld==max(avg_most_steps$gemiddeld))
```
The maximum steps are 206.1698113 at interval 835.


### 6 Code to describe and show a strategy for imputing missing data


First of all w heave to check if and where missing data is.
So we check all variables if any missing data exists

```r
s<-anyNA(activity$steps)
d<-anyNA(activity$date)
i<-anyNA(activity$interval)
```
Are there NA's in activity$step TRUE    

Are there NA's in activity$date FALSE  

Are there NA's in activity$interval FALSE  

Because this is a timeseries, I choose for the imputeTS package

First find out where the missing data is:

```r
#where are the missing data
ggplot_na_distribution(activity$steps)
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Ok now its time to see what different impute methods are doing

```r
imp_mean <- na_mean(activity$steps)
p1<-ggplot_na_imputations(activity$steps, imp_mean, subtitle= "Visualization of missing value replacements\nmethod=mean")
imp_interpolation <- na_interpolation(activity$steps)
p2<-ggplot_na_imputations(activity$steps, imp_interpolation, subtitle= "Visualization of missing value replacements\nmethod=interpolation")
imp_kalman <- na_kalman(activity$steps)
p3<-ggplot_na_imputations(activity$steps, imp_kalman, subtitle= "Visualization of missing value replacements\nmethod=kalman")
imp_ma <- na_ma(activity$steps)
p4<-ggplot_na_imputations(activity$steps, imp_ma, subtitle= "Visualization of missing value replacements \nmethod=ma")
#Overview of plots
grid.arrange(p1, p2, p3, p4, nrow = 2)
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-11-1.png)<!-- -->


It looks like the method mean is the best method for imputing, so that will it be.

```r
imp_mean <- na_mean(activity$steps)
ggplot_na_imputations(activity$steps, imp_mean, subtitle= "Visualization of missing value replacements, method=mean")
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### 7 Histogram of the total number of steps taken each day after missing values are imputed


```r
activity_imp<-cbind(activity, imp_mean)
steps_each_day_imp<-activity_imp%>%
                group_by(date)%>%
                summarise_at(vars(imp_mean),list(sum=sum))
ggplot(data=steps_each_day_imp, aes(sum)) + 
      geom_histogram(binwidth = 2000)+ 
      labs(title="Total number of steps taken each day") +
      labs(x="Total steps",y="Freqency")
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
mediaan_imp<-median(steps_each_day_imp$sum, na.rm=TRUE)
gemiddeld_imp<-round(mean(steps_each_day_imp$sum, na.rm=TRUE),2)
```
The median before imputing: 10765 and after imputing 10766.1886792
The mean before imputing: 10766.19 and after imputing 10766.19



### 8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
avg_most_steps_comp<-activity%>%   
                     mutate(weekday= ifelse((wday(date)==1 | wday(date)==7), "weekends", "weekdays")) %>%
                     group_by(weekday,interval) %>%                 
                     summarise_at(vars(steps), list(gemiddeld = mean), na.rm=TRUE)
d <- ggplot(avg_most_steps_comp, aes(interval,gemiddeld)) + geom_line() 
d + facet_wrap(~ weekday,  dir = "v")+ labs(title="Average number of steps taken per 5-minute interval ")
```

![](Reproducible_Research_Assignment_Week2._files/figure-html/unnamed-chunk-14-1.png)<!-- -->


### 9 All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

