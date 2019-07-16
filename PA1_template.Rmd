---
title: "Reproducible Research Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

Loading and preprocessing the data
## Loading and preprocessing the data
```{r mean, echo=TRUE}
activity<-read.csv("activity.csv",header = TRUE) ##Reading File
activity$date<-as.POSIXct(activity$date,"%Y-%m-%d")
weekday<-weekdays(activity$date)
activity<-cbind(activity,weekday)

summary(activity)

total_steps<-with(activity, aggregate(activity$steps, by=list(date),FUN=sum, na.rm= TRUE ))

names(total_steps)=c("date","steps")

hist(total_steps$steps,main = "Total no of steps taken in a day", xlab = "Total steps taken per day", col = "Blue", ylim = c(0,20),breaks = seq(0,25000, by=2500)) ##Plot
## What is mean total number of steps taken per day?
mean(total_steps$steps)
median(total_steps$steps)
```
```{r dailyaverage, echo=TRUE}
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
## What is the average daily activity pattern?
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```
```{r imputed, echo=TRUE}
sum(is.na(activity$steps))
## Imputing missing values
imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]

activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")

hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

mean(total_steps_imputed$daily_steps)

median(total_steps_imputed$daily_steps)
```

```{r weekday, echo=TRUE}
library(ggplot2)
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
## Are there differences in activity patterns between weekdays and weekends?
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```