# Required libraries
library(lubridate)
library(beanplot)
library(doBy)
library(modeest)
library(plyr)
library(psych)

# We can each add in our working directories here - just un# and # as you check code out and back in
# setwd("C:/Users/Jim Braun/My Documents/Predict 498 Capstone/Data Mining Cup")
#
#
#

# Read in data from Google Drive
# Need to update path
orders.train <- read.table("C:/Users/Katie/Google Drive/Predict 498 Capstone/orders_train.txt", header = TRUE, sep = ";")
# Jim's path
orders.train <- read.table("C:/Users/Jim Braun/My Documents/Predict 498 Capstone/Data Mining Cup/orders_train.txt", header = TRUE, sep = ";")
7library(tseries)7
library(forecast)

# Read in data from Google Drive
# Added the 
orders.train <- read.table("orders_train.txt", header = TRUE, sep = ";")
#orders.train <- read.table("C:/Users/Katie/Google Drive/Predict 498 Capstone/orders_train.txt", header = TRUE, sep = ";")
# orders.train <- read.table("C:/Users/Jim Braun/My Documents/Predict 498 Capstone/Data Mining Cup/orders_train.txt", header = TRUE, sep = ";")
str(orders.train)

# Update date fields to date type instead of factors
orders.train$orderDate <- as.Date(orders.train$orderDate, format = "%Y-%m-%d")
orders.train$deliveryDate <- as.Date(orders.train$deliveryDate, format = "%Y-%m-%d")
orders.train$dateOfBirth <- as.Date(orders.train$dateOfBirth, format = "%Y-%m-%d")
orders.train$creationDate <- as.Date(orders.train$creationDate, format = "%Y-%m-%d")
str(orders.train)

summary(orders.train)

# Add date diff variables
orders.train$timeToDeliver <- as.numeric(difftime(orders.train$deliveryDate,orders.train$orderDate,unit="days"))
orders.train$accountAge <- as.numeric(difftime(orders.train$orderDate,orders.train$creationDate,unit="weeks"))/52.25
orders.train$customerAge <- as.numeric(difftime(orders.train$orderDate,orders.train$dateOfBirth,unit="weeks"))/52.25

# Check
summary(orders.train[15:17])

# timeToDeliver should never be negative, and age should never be negative
orders.train$timeToDeliver <- ifelse(orders.train$timeToDeliver<0,NA,orders.train$timeToDeliver)
orders.train$customerAge <- ifelse(orders.train$customerAge<0,NA,orders.train$customerAge)
# age should also probably not be > 100 - what should we use for the cut-off?
orders.train$customerAge <- ifelse(orders.train$customerAge>100,NA,orders.train$customerAge)

# Recheck
summary(orders.train[15:17])

# Look at PDF of numeric variables given reponse
# Note that we're just using a random sample due to processing time for graphics
set.seed(498)
sample_ind <- sample(seq_len(nrow(orders.train)), size = 1000)
orders.sample <- orders.train [sample_ind, ]
beanplot(customerAge ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Customer Age Distribution", ylab = "Age in Years", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(accountAge ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Account Age Distribution", ylab = "Age in Years", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(timeToDeliver ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Delivery Time Distribution", ylab = "Time in Days", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(price ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Price Distribution", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))

# Mean & count of response given nominal vars
# Only doing ones with few possible values- salutation & state
summaryBy(returnShipment ~ salutation, orders.train, FUN=c(length,mean))
summaryBy(returnShipment ~ state, orders.train, FUN=c(length,mean))

# More EDA - a breakout of stats by returnShipment
describeBy(orders.train, group=orders.train$returnShipment, mat=FALSE, type=3, digits=6)

# quick X vs Y plot
plot(orders.sample, cex=0.1)

# calculate customer's preferred size
# this was WAY more complicated than necessary...
# mvf = most frequent value (a.k.a mode), requires Modeest package and library
# Crap - have to make # obs match orders.sample
# also, why does this create 3 variables instead of 1?
custMode <- summaryBy(size ~ customerID, data=orders.sample, FUN = function (x) {c(m=mfv(x))})
custMode

custMode <- customer

  # sorting orders by customerID to cbind customer Mode to right observation
  r <- order(orders.sample$customerID)
  r
  sortID <- orders.sample[r,]
  sortID
cbind(sortID,custMode[,2])

# Add column to denote whether the order size was not the customer's usual order (size mode)
# had to use custMode column instead of one cbinded in. Not sure why, but this works 
sortID$OrdNotMode <- ifelse((sortID$size != custMode[,2]),0,1)
sortID$OrdNotMode

     beanplot(sortID$OrdNotMode ~ returnShipment, sortID, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Unusual Size?", xaxt="n")
     legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))

# let's try this again...
#nope
mfv(orders.sample$size, group=orders.sample$customerID)
mfv(orders.sample$size)

#nope
myfun<-function(x){mfv(x)}
  summaryBy(orders.sample$size~orders.sample$customerID, data=orders.sample, FUN=myfun)

# nope
OB <- orderBy(~orders.sample$customerID+orders.sample$size, data=orders.sample)
  OM <- function(d){c(NA,mfv(orders.sample$size)}
  v<-lapplyBy(~orders.sample$customerID, data=orders.sample, OM)
  orders.sample$OM <-unlist(v)

# Try this one for modes- but do we need to get a numeric and s/m/l?
# First convert from a factor to a string, standardizing case
orders.train$revSize <- toupper(as.character(orders.train$size))
# Add mode function - note that this only gives one mode if there is more than one
mymode <- function(x){
  names(sort(-table(as.character(x))))[1]
}
custMode <- summaryBy(revSize ~ customerID, orders.train, FUN=mymode)

# Time-series data - taking the mean of return aggregated by order date
# NOTE- it's been awhile since I've done a TS analysis, so really I was just looking at the plots & packages here. It will likely need a fair bit of revisions.
avgReturnByDay <- summaryBy(returnShipment ~ orderDate, orders.train, FUN=mean)
ts.orders <- ts(avgReturnByDay$returnShipment.mean, start=c(2012,4), frequency=365)
plot(ts.orders)
acf(ts.orders,20)
pacf(ts.orders,20)
lag.plot(ts.orders,9,do.lines=F)
plot(diff(ts.orders))
acf(diff(ts.orders),20)
pacf(diff(ts.orders),20)
adf.test(ts.orders)
auto.arima(ts.orders)
