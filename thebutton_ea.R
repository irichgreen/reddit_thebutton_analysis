# Data Visualization and Analysis of Reddit's "The Button" Data
# Myles Harrison
# June 2015
# http://www.everydayanalytics.ca/2015/06/data-visualization-and-analysis-of-the-button-reddit.html

# REQUIREMENTS
library(ggplot2)
library(reshape)
library(dplyr)
library(RColorBrewer)

# DATA INPUT & MASSAGE
# Read in the data 
data <- read.csv(file="thebutton_presses.csv", header=T, stringsAsFactors=FALSE)

# Parse timestamps in date
data$press.time <- strptime(data$press.time, format="%Y-%m-%dT%H:%M:%S")

# Blank flair & non-pressers - convert to -1
seconds <- data$flair
seconds[which(data$flair=="non presser")] <- -1
seconds[which(data$flair=="")] <- -1

# Convert flair to numerics
seconds <- as.numeric(sub("s", "", seconds))
data <- data.frame(data, seconds)
names(data)[5] <- "seconds"

# Tidy CSS classes
data <- data[-which(data$css==""),] #blank
data$css <- gsub("-", "", data$css) #remove dashes

# ANALYSIS & VISUALIZATION
# IN TIME
# Presses per second - OK
pps <- aggregate(seconds ~ as.character(press.time), data=data, FUN=length)
# cast back to timestamp
names(pps) <- c("time", "count")
pps$time <- as.POSIXct(pps$time)
ggplot(pps, aes(x=time, y=count)) + geom_line() + ggtitle('Button Clicks per Second')

# Button clicks per day
ppd <- aggregate(seconds ~ as.Date(press.time), data=data, FUN=length)
names(ppd) <- c("date", "count")
ggplot(ppd, aes(x=date, y=count)) + geom_line() + scale_y_continuous() + ggtitle('Button Clicks per Day')
ggplot(ppd, aes(x=date, y=count)) + geom_line() + scale_y_continuous(trans='log', breaks=c(2500,5000,1E4,2.5E4, 5E4, 1E5, 2.5E5, 3E5)) + ggtitle('Button Clicks per Day')

# Button clicks per hour
hourly1 <- aggregate(seconds ~ strftime(press.time, "%Y-%m-%d %H"), data=data, FUN="length")
names(hourly1) <- c("time", "count")
hourly1$time <- strptime(hourly1$time, format="%Y-%m-%d %H")
#plot(hourly1$time, hourly1$count, type='l', main='Button Clicks by Hour', xlab='Time', ylab='Clicks')
ggplot(hourly1, aes(x=time, y=count)) + geom_line() + ggtitle('Clicks by Hour')
ggplot(hourly1[-seq(0:48),], aes(x=time, y=count)) + geom_line() + ggtitle('Clicks by Hour')
ggplot(hourly1, aes(x=time, y=count)) + geom_line() + ggtitle('Clicks by Hour') + scale_y_continuous(trans='log', breaks=c(100,500,1000,2500,5000,1E4,5E4))

# Hourly overall
hourly2 <- table(strftime(data$press.time, "%H"))
hourly2 <- data.frame(hourly2)
names(hourly2) <- c("hour", "count")
ggplot(hourly2, aes(x=hour,y=count)) + geom_bar(stat="identity") + ggtitle("Clicks by Hour of Day (Total)")

# Hourly overall heatmap
hourly_overall_hm <- aggregate(css ~ strftime(press.time, "%H")*strftime(press.time, "%A"), data=data, FUN=length)
names(hourly_overall_hm) <- c("hour", "weekday", "count")
weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
hourly_overall_hm$weekday <- factor(hourly_overall_hm$weekday, levels=weekdays, ordered=TRUE)
ggplot(hourly_overall_hm, aes(x=weekday, y=hour, fill=count)) + geom_tile() +scale_fill_gradient(trans='log', breaks=c(100,500,1000,2500,5000,1E4,2.5E4,5E4)) + ggtitle('Clicks by Hour of Day Heatmap (Overall)')

# Hourly (heatmap)
hourly_hm <- aggregate(css ~ strftime(press.time, "%H")*as.Date(press.time), data=data, FUN=length)
names(hourly_hm) <- c("hour", "date", "count")
ggplot(hourly_hm, aes(x=date, y=hour, fill=count)) + geom_tile() + scale_fill_gradient(trans='log', breaks=c(100,500,1000,2500,5000,1E4,5E4)) + ggtitle('Clicks by Hour of Day Heatmap')
ggplot(hourly_hm[-c(0:56),], aes(x=date, y=hour, fill=count)) + geom_tile() + scale_fill_gradient(trans='log', breaks=c(100,500,1000,2500,5000,1E4,5E4)) + ggtitle('Clicks by Hour of Day Heatmap')
# Alternate colouring
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)
ggplot(hourly_hm, aes(x=date, y=hour, fill=count)) + geom_tile() + scale_fill_gradientn(trans='log', breaks=c(100,500,1000,2500,5000,1E4,5E4), colours=myPalette) + ggtitle('Clicks by Hour of Day Heatmap')
ggplot(hourly_hm[-c(0:56),], aes(x=date, y=hour, fill=count)) + geom_tile() +scale_fill_gradientn(colours=myPalette) + ggtitle('Clicks by Hour of Day Heatmap')

# Percentage of presses per day by hour
hourly_hm2 <- mutate(group_by(hourly_hm,date),percent=count/sum(count)*100.0)
ggplot(hourly_hm2, aes(x=date, y=hour, fill=percent)) + geom_tile() +scale_fill_gradientn(colours=myPalette) + ggtitle('Percentage of Daily Clicks by Hour of Day Heatmap')

# BY CSS TYPE AND TIME REMAINING
# Histogram of presses per second
#ggplot(pps, aes(count)) + geom_histogram() + ggtitle('Histogram of Clicks per Second') + xlab('Clicks per second')
#ggplot(pps, aes(count)) + geom_histogram() + scale_y_continuous(trans='log',breaks=c(1,100,500,1000,2500,5000,1E4,5E4,1E5,5E5)) + ggtitle('Histogram of Clicks per Second') + xlab('Clicks per second')

# Bar plot of clicks per css type
ggplot(data, aes(x=css)) + geom_bar() + ggtitle('Clicks per CSS type')

# This is roughly equivalent to a histogram of width 10 using the flair
ggplot(data, aes(x=seconds)) + geom_histogram(breaks=c(0,seq(0:5)*10+1))

# More granular histogram
ggplot(data, aes(x=seconds)) + geom_histogram(breaks=seq(0:61)) + ggtitle("Histogram of Button Clicks by Time Remaining")
ggplot(data, aes(x=seconds)) + geom_histogram(breaks=seq(0:61)) + ggtitle("Histogram of Button Clicks by Time Remaining") + scale_y_continuous(trans='log')

# Cheater css applied to many time intervals
ggplot(data, aes(x=css,y=seconds)) + geom_boxplot() + ggtitle('Boxplot by CSS type')
ggplot(data, aes(x=css,y=seconds)) + geom_jitter(size=0.1) + ggtitle('Jitter plot by CSS type')

# Presses by hour and seconds left
pth <- aggregate(flair ~ seconds*strftime(press.time, "%Y-%m-%d %H"), data=data, FUN=length)
names(pth) <- c("seconds", "hour", "count")
pth$hour <- strptime(pth$hour, format="%Y-%m-%d %H")
ggplot(pth, aes(x=hour, y=seconds, fill=count)) + geom_tile() + scale_fill_gradient(trans='log', breaks=c(1,10,100,1000,10000,100000))

# Presses by time and second - finer granularity - raw counts
pts <- aggregate(flair ~ seconds*as.Date(press.time), data=data, FUN=length)
names(pts) <- c("seconds", "date", "count")
ggplot(pts, aes(x=date, y=seconds, fill=count)) + geom_tile() + scale_fill_gradient(trans='log', breaks=c(1,10,100,1000,10000,100000)) + ggtitle('Button presses per Day by Seconds Left')
ggplot(pts, aes(x=date, y=seconds, fill=count)) + geom_tile() + scale_fill_gradientn(colours=myPalette, trans='log', breaks=c(1,10,100,1000,10000,100000)) + ggtitle('Button presses per Day by Seconds Left')
# Exclude earlier data days
ggplot(pts[-seq(0:1000),], aes(x=date, y=seconds, fill=count)) + geom_tile() + scale_fill_gradient()
ggplot(pts[-seq(0:1000),], aes(x=date, y=seconds, fill=count)) + geom_tile() + scale_fill_gradientn(colours=myPalette)

# Percentage of press by time remaining per day
pts3 <- mutate(group_by(pts, date), percent=count/sum(count)*100.0)
ggplot(pts3, aes(x=date, y=seconds, fill=percent)) + geom_tile() + scale_fill_gradientn(colours=myPalette)
ggplot(pts3[-seq(0:24),], aes(x=date, y=seconds, fill=percent)) + geom_tile() + scale_fill_gradientn(colours=myPalette)

# PERCENTAGE OF CSS TYPE PER DAY
cssbyday <- aggregate(flair ~ css*as.Date(press.time), data=data, FUN=length)
names(cssbyday) <- c("css", "date", "count")

# Percentage
ggplot(cssbyday, aes(x=date, y=count, group=css, color=css)) + geom_area(position="fill",aes(color=css, fill=css)) + ggtitle("Proportion of Daily Clicks by CSS Type") + ylab("Proportion") + xlab("Date") + scale_colour_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")

# Line graph by CSS type
ggplot(cssbyday, aes(x=date, y=count, group=css, color=css)) + geom_line() + scale_y_continuous(trans='log', breaks=c(1, 100, 1000, 5000, 1E4, 5E4, 1E5, 2.5E5)) + geom_point() + ggtitle('Button presses per Day by CSS Type') + xlab("Date") + scale_colour_brewer(palette="Set1")
