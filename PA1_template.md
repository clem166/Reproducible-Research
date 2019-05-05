---
title: "Untitled"
author: "Clement Yeung"
date: "May 5, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r data_read, results="hide", warning=FALSE, message=FALSE}
# loading packages
library(dplyr)
library(lubridate)
library(ggplot2)

#set wd
setwd("C:/Users/theGameTrader/Documents/R/coursera/reproducible research")
#reading in data
activity= read.csv("./activity.csv")
#formatting date to be a date variable instead of factor
activity$date = ymd(activity$date) 
```
```{r summary}
#analysis to check data type
head(activity)
str(activity)
```

```{r}

#calculating sum, mean and median number of steps
activity_summary =  activity %>%
                    group_by(date) %>%
                    summarise(sum=sum(steps, na.rm=TRUE), mean=mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE) )

#histogram
hist(activity_summary$sum)

#calulating mean number of steps by time interval
activity_summary2 = activity %>%
                    group_by(interval) %>%
                    summarise(mean=mean(steps, na.rm=TRUE))

#time series plot
ggplot(data=activity_summary2, aes(x=interval, y=mean)) +
  geom_line(color ="red", size=1)
#maximium 
max(activity_summary2$mean)
activity_summary2[order(-activity_summary2$mean), ]
#retrieving max value
activity_summary2[which.max(activity_summary2$mean),]

#calculating NAs by column
activity %>%
  summarise_all(funs(sum(is.na(.))))
#double check
sum(is.na(activity))

#replace missing with mean by interval
#replace(variable to replace, list of things to replace, value with which to replace with)
activity2= activity %>% group_by(interval) %>% mutate_all(funs(replace(., which(is.na(.)),
                                                             mean(., na.rm=TRUE))))
new_summary = activity2 %>%
              group_by(date) %>%
              summarise(sum=sum(steps, na.rm=TRUE), mean=mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE) )

head(activity_summary)
head(new_summary)
par(mfrow=c(1,2))

hist(activity_summary$sum)
hist(new_summary$sum)

#Setting weekday/ weekends
activity2$day = weekdays(activity2$date)
activity2$day_type = ifelse(activity2$day %in% c("Saturday","Sunday"), "weekend", "weekday")
#check
activity2 %>% group_by(day_type, day) %>% summarise(n=n())

#weekday summray
split_summary = activity2 %>%
                group_by(interval, day_type) %>%
                summarise(sum=sum(steps, na.rm=TRUE), mean=mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE) )

par(mfrow=c(2,1))

ggplot(data=split_summary, aes(x=interval, y=mean, group=day_type, colour=day_type)) +
  geom_line() +
  facet_grid(~day_type,scales="free")

```