library(lubridate)
library(stringr)
setwd("~/R/Reproducible Research")
activity<-read.csv("activity.csv", sep=",")
#days 
date<-unique(activity$date)
i<-rep(0,length(date))
stepsdays<-rep(0,length(date))
#total steps per day
for(i in 1:length(date)){stepsdays[i]<-sum(activity[activity$date==date[i],]$steps,na.rm = TRUE)}



##Mean of steps of days


mean(stepsdays)

median(stepsdays)




hist(stepsdays,n=20)
time<-(strptime(str_pad(as.character(activity[,3]),4,side="left",pad="0"),"%H%M"))
dattee<-strptime(as.character.Date(activity[,2]),"%Y-%m-%d")
year(time)<-year(dattee)
day(time)<-day(dattee)
month(time)<-month(dattee)
plot(time,activity$steps,type = "l")

datte<-strptime(as.character.Date(date),"%Y-%m-%d")
plot(datte,stepsdays,type="l")



wdat<-weekdays(datte)
wend<-as.factor(!(weekdays(datte) %in% c('duminicã','sâmbãtã')))

par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(datte[wend==TRUE],stepsdays[wend==TRUE],col="red")
plot(datte[wend==FALSE],stepsdays[wend==FALSE],col="blue")

interval<-unique(activity$interval)
j<-0
i<-0
activity1<-activity
intervalmean<-rep(0,length(interval))
in
for( j in 1:length(interval)){intervalmean[j]<-mean(activity[activity$interval==interval[j],]$steps,na.rm=TRUE)}
intervalmean<-as.data.frame(cbind(intervalmean,interval))


for(i in 1:length(activity1$steps))
    {if(is.na(activity1$steps[i]))
    {
          activity1$steps[i]<-intervalmean[intervalmean$interval==activity1$interval[i],]$intervalmean} 
      }

stepsdays1<-rep(0,length((date)))
for(i in 1:length(date)){stepsdays1[i]<-sum(activity[activity1$date==date[i],]$steps,na.rm = TRUE)}


