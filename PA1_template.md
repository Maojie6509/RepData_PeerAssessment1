
##Loading the data,read the data in R, the activity.csv file, assigning NA to missing values


newData <- read.csv(file="activity.csv", header = TRUE, na.strings = "NA")
##Checking the dataset

head(newData)
class(newData$date)
newData$date <- as.Date(newData$date, format = "%Y-%m-%d")
class(newData$date)
sum(is.na(newData$step))

##Creating a new dataset removing all missing values:

removed_NA_StepData <- newData[!is.na(newData$step), ]
##checking the new dataset

head(removed_NA_StepData)

## split the dataset into diferent days:
splitDataByDate <- split(removed_NA_StepData$steps, removed_NA_StepData$date)

## with all days in dataset:
allDays <- as.Date(sort(unique(removed_NA_StepData$date)), format = "%Y-%m-%d")

## calculate sum, mean and median of all step splitted by day:
sumStepByDate <- as.data.frame(sapply(splitDataByDate, sum))
names(sumStepByDate) <- c("Steps")
mean(sumStepByDate$Steps)
## [1] 10766
median(sumStepByDate$Steps)
## [1] 10765
## generate a histogram with the frecuency of total sum of steps by day:

hist(sumStepByDate$Steps, main = "Total steps by day frecuency", xlab = "Number of steps by day", 
    ylab = "Number of days", col = "red", breaks = nrow(sumStepByDate))
plot of chunk unnamed-chunk-12

## save it into '/figures/' path:

png(filename = "figures/plot1.png", width = 480, height = 480, units = "px")
hist(sumStepByDate$Steps, main = "Total steps by day frecuency", xlab = "Number of steps by day", 
    ylab = "Number of days", col = "red", breaks = nrow(sumStepByDate))
dev.off()
## pdf 
##   2
## create a file with the sum of steps day by day, the mean and the median of all days in dataset:

plot(allDays, sumStepByDate$Steps, type = "l", ylab = "Sum of step", xlab = "Day")
lines(allDays, rep(median(sumStepByDate$Steps), length(allDays)), type = "l", 
    col = "blue")
lines(allDays, rep(mean(sumStepByDate$Steps), length(allDays)), type = "l", 
    col = "red")
legend("topright", c("Mean", "Median"), lty = 1, col = c("red", "blue"), cex = 0.95)
plot of chunk unnamed-chunk-14

png(filename = "figures/plot2.png", width = 480, height = 480, units = "px")
plot(allDays, sumStepByDate$Steps, type = "l", ylab = "Sum of step", xlab = "Day")
lines(allDays, rep(median(sumStepByDate$Steps), length(allDays)), type = "l", 
    col = "blue")
lines(allDays, rep(mean(sumStepByDate$Steps), length(allDays)), type = "l", 
    col = "red")
legend("topright", c("Mean", "Median"), lty = 1, col = c("red", "blue"), cex = 0.95)
dev.off()
## pdf 
##   2
#mean:
mean(sumStepByDate$Steps)
## [1] 10766
#Median:
median(sumStepByDate$Steps)
## [1] 10765
## make a Interval segmentation:

allIntervals <- sort(unique(removed_NA_StepData$interval))
splitDataByInterval <- split(removed_NA_StepData$steps, removed_NA_StepData$interval)
##calculate the mean for each interval..

meanStepByInterval <- as.data.frame(sapply(splitDataByInterval, mean))
names(meanStepByInterval) <- c("Steps")
##Drawing the solution:

plot(allIntervals, meanStepByInterval$Steps, type = "l", main = "5-Interval step mean", 
    xlab = "5-interval", ylab = "Mean of steps")
lines(allIntervals, rep(max(meanStepByInterval$Steps), length(allIntervals)), 
    type = "l", col = "red")
legend("topright", c("Max"), lty = 1, col = c("red"), cex = 0.95)


png(filename = "figures/plot3.png", width = 480, height = 480, units = "px")
plot(allIntervals, meanStepByInterval$Steps, type = "l", main = "5-Interval step mean", 
    xlab = "5-interval", ylab = "Mean of steps")
lines(allIntervals, rep(max(meanStepByInterval$Steps), length(allIntervals)), 
    type = "l", col = "red")
legend("topright", c("Max"), lty = 1, col = c("red"), cex = 0.95)
dev.off()

##Maxim mean of step in an 835 5-minutes interval:

max(meanStepByInterval$Steps)

##Imputing missing values

simStepData <- newData
##Calculate the list of means split by date:

meanSByDate <- sapply(splitDataByDate, mean)
Fill in the NA values with the mean of the day:

for (i in 1:nrow(simStepData)) {
    if (is.na(simStepData[i, 1])) {
        date_i <- simStepData[i, 2]
        simStepData[i, 1] <- meanSByDate[[as.factor(date_i)]]
    }
}

splitSimulatedDataByDate <- split(simStepData$steps, simStepData$date)

sumSimulaStepByDate <- as.data.frame(sapply(splitSimulatedDataByDate, sum))
names(sumSimulaStepByDate) <- c("Steps")
mean(sumSimulaStepByDate$Steps)

median(sumSimulaStepByDate$Steps)


##Drawing the solution:

hist(sumSimulaStepByDate$Steps, main = "Total steps by day frecuency", xlab = "Number of steps by day", 
    ylab = "Number of days", col = "red", breaks = nrow(sumSimulaStepByDate))
plot of chunk unnamed-chunk-27

png(filename = "figures/plot4.png", width = 480, height = 480, units = "px")
hist(sumSimulaStepByDate$Steps, main = "Total steps by day frecuency", xlab = "Number of steps by day", 
    ylab = "Number of days", col = "red", breaks = nrow(sumSimulaStepByDate))
dev.off()

allSimDays <- as.Date(sort(unique(simStepData$date)), format = "%Y-%m-%d")
plot(allSimDays, sumSimulaStepByDate$Steps, type = "l", ylab = "Sum of step", 
    xlab = "Day")
lines(allSimDays, rep(median(sumSimulaStepByDate$Steps), length(allSimDays)), 
    type = "l", col = "blue")
lines(allSimDays, rep(mean(sumSimulaStepByDate$Steps), length(allSimDays)), 
    type = "l", col = "red")
legend("topright", c("Mean", "Median"), lty = 1, col = c("red", "blue"), cex = 0.95)
plot of chunk unnamed-chunk-30

png(filename = "figures/plot5.png", width = 480, height = 480, units = "px")
plot(allSimDays, sumSimulaStepByDate$Steps, type = "l", ylab = "Sum of step", 
    xlab = "Day")
lines(allSimDays, rep(median(sumSimulaStepByDate$Steps), length(allSimDays)), 
    type = "l", col = "blue")
lines(allSimDays, rep(mean(sumSimulaStepByDate$Steps), length(allSimDays)), 
    type = "l", col = "red")
legend("topright", c("Mean", "Median"), lty = 1, col = c("red", "blue"), cex = 0.95)
dev.off()


## create a new variable in dataset called 'dateWeek' and put the value of day of the week (weekdays function)

simStepData$dateWeek <- weekdays(simStepData$date)
##compare if dateWeek is 'saturday' or 'sunday' ( 'sábado' ó 'domingo' in my Spanish version of OS) and put the correct factor ('Weekend or Weekday') depend on it.

for (i in 1:nrow(simStepData)) {
    if (simStepData[i, 4] == "domingo" | simStepData[i, 4] == "sábado") {
        simStepData[i, 4] <- "Weekend"
    } else {
        simStepData[i, 4] <- "Weekday"
    }
}
##split the dataset by dateWeek and then separate inte two diferent datasets:

weekDaySplit <- split(simStepData, simStepData$dateWeek)
weekendData <- weekDaySplit[["Weekend"]]
weekdayData <- weekDaySplit[["Weekday"]]
First Method: basic system graph

##create an other split in both datasets by Interval

allweekendIntervals <- sort(unique(weekendData$interval))
splitweekendDataByInterval <- split(weekendData$steps, weekendData$interval)

allweekdayIntervals <- sort(unique(weekdayData$interval))
splitweekdayDataByInterval <- split(weekdayData$steps, weekdayData$interval)
..and calculate the mean of steps for each interval..

meanWeekendStepByInterval <- as.data.frame(sapply(splitweekendDataByInterval, 
    mean))
names(meanWeekendStepByInterval) <- c("Steps")

meanWeekdayStepByInterval <- as.data.frame(sapply(splitweekdayDataByInterval, 
    mean))
names(meanWeekdayStepByInterval) <- c("Steps")
Drawing the solution:

Weekend

plot(allIntervals, meanWeekendStepByInterval$Steps, type = "l", col = "blue", 
    main = "Weekend. steps mean", xlab = "5-interval", ylab = "Mean of steps")
plot of chunk unnamed-chunk-37

png(filename = "figures/plot6.png", width = 480, height = 480, units = "px")
plot(allIntervals, meanWeekendStepByInterval$Steps, type = "l", col = "blue", 
    main = "Weekend. steps mean", xlab = "5-interval", ylab = "Mean of steps")
dev.off()
## pdf 
##   2
Weekday

plot(allIntervals, meanWeekdayStepByInterval$Steps, type = "l", col = "blue", 
    main = "Weekday. steps mean", xlab = "5-interval", ylab = "Mean of steps")
plot of chunk unnamed-chunk-39

png(filename = "figures/plot7.png", width = 480, height = 480, units = "px")
plot(allIntervals, meanWeekdayStepByInterval$Steps, type = "l", col = "blue", 
    main = "Weekend. steps mean", xlab = "5-interval", ylab = "Mean of steps")
dev.off()
## pdf 
##   2
