setwd("C:/Users/skao/Documents/DataVizzy/sleep/data/")
filename <- "sleep.csv"

data <- read.csv("sleep.csv")

# Data Cleaning and Manipulation========================================================

nrow(data)
head(data,2)
data[,c(2,3,4,5)] <- lapply(data[c(2,3,4,5)],FUN=gsub,pattern=" ", replacement="")
data$R.Date <- as.Date(data$Date, "%m/%d/%Y")
data$R.Bed.Time <- strptime(paste(data$Date, data$Bed.Time, sep=" "), "%m/%d/%Y  %I:%M%p")
before.mid <- which(substr(data$Bed.Time,1,2) %in% c("11","10", "9:","8:", "7:"))
data$R.Sleep.Onset.Time <- strptime(paste(data$Date, data$Sleep.Onset.Time,sep=" "), "%m/%d/%Y   %I:%M%p")
after.mid <- which(substr(data$Sleep.Onset.Time,1,2) %in% c("12","1:","2:","3:", "4:", "5:","6:"))
data$R.Sleep.Onset.Time[intersect(before.mid,after.mid)] <- data$R.Sleep.Onset.Time[intersect(before.mid,after.mid)] + 60*60*24
data$R.Ring.Time <- strptime(paste(data$Date, data$Ring.Time,sep=" "), "%m/%d/%Y   %I:%M%p")
data$R.Ring.Time[before.mid] <- data$R.Ring.Time[before.mid] + 60*60*24
data$R.Wake.Time <- strptime(paste(data$Date, data$Wake.Time,sep=" "), "%m/%d/%Y   %I:%M%p")
data$R.Wake.Time[before.mid] <- data$R.Wake.Time[before.mid] + 60*60*24
data$R.Time.In.Bed <- difftime(data$R.Wake.Time, data$R.Bed.Time, units="hours")
data$R.Total.Sleep.Time <- difftime(data$R.Ring.Time, data$R.Sleep.Onset.Time, units="hours")
data$R.Total.Awake.Time <- difftime(data$R.Sleep.Onset.Time, data$R.Bed.Time, units="min") + difftime(data$R.Wake.Time, data$R.Ring.Time,units="min")
data$R.Sleep.Onset.Latency <- difftime(data$R.Sleep.Onset.Time, data$R.Bed.Time, units=mins)
data$R.Sleep.Efficiency <- as.numeric(substr(data$Sleep.Efficiency,1,4))

# EDA ========================================================

head(data, 10)
colnames(data)
weekday.static <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
master.dates <- data.frame(seq(as.Date("2014-01-01","%Y-%m-%d"), as.Date("2014-08-25","%Y-%m-%d"), by =1))
colnames(master.dates) <- "R.Date"
master.data <- merge(master.dates, data, by="R.Date",all=TRUE)
master.data$dayofweek <- factor(weekdays(master.data$R.Date), weekday.static)

head(master.data,22)
plot(master.data$R.Date, master.data$Sleep.Efficiency, type="b", cex = 1)


# Day of week averages
agg.by <- master.data$R.Time.In.Bed
agg.by <- master.data$R.Total.Sleep.Time
agg.by <- master.data$R.Total.Awake.Time
agg.by <- master.data$R.Sleep.Onset.Latency
agg.by <- master.data$R.Sleep.Efficiency
agg.by <- master.data$Snooze
agg.by <- master.data$Halfway.Awakening

averages <- aggregate(master.data[,c(11,12,18:22)], by=list(master.data$dayofweek),FUN=mean, na.rm=TRUE)

plot(averages)


