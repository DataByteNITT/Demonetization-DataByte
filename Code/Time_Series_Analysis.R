# import packages
library(ggplot2)
library(plyr)

# set delta as 10 mins
dt <- 10 

# Read the dataset into a variable
tweetsjsr <- read.csv("D:/1 DATA BYTE/1_projects/demonetization proj/from shivani/DataSets/demonetization-tweets.csv")
# remove duplicate records
tweets <- unique(tweetsjsr)
# name the columns
names(tweets) <- c("sno","text","favtd" ,"favcount","replysn", "date","trunc","replysid","id","replyuid","statussrc","screenname","rtcount", "isrt","rtd")

# convert date from factors form to date format
tweets$date <- as.POSIXct(tweets$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")

# Build datebreaks
# find the first tweet
minDate <- min(tweets$date)
# find the last tweet
maxDate <- max(tweets$date) + 60 * dt
# set the breaks for time series analysis
dateBreaks <- seq(minDate, maxDate, by=60 * dt)

# Use hist to count the number of tweets per bin but don't plot.
tweetCount <- hist(tweets$date, breaks=dateBreaks, plot=FALSE)
# Strip out the left endpoint of each bin.
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]
# Count number of unique tweeters per bin.
userCount <- sapply(binBreaks, function(d) length(unique(tweets$user[which((tweets$date >= d) & (tweets$date <= d + 60*dt))]))) 
# creating data to be plotted
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count), users=as.numeric(userCount))
# customizing the plot
ggplot(plotData) + 
  geom_bar(aes(x=dates, y=tweets, color=users), stat="identity") +
  scale_x_datetime("Date") +
  scale_y_continuous("Number of tweets") +
  labs(title = "Number of tweets and unique users : #demonetization")
