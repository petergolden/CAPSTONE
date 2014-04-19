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

#--------------------------#
# DEAL WITH MISSING VALUES #
#--------------------------#

# using mi package - get visual plot of missing obs
pdf(file = "missing_obs_plots.pdf", width = 11, height = 8.5)  ##/\open pdf/\##
missing.pattern.plot(orders.train, gray.scale = TRUE)
dev.off()										##\/close pdf\/##

# check how many observations for each variable have missing values
sum(is.na(orders.train$variable_names))

#--------------------------#
#      Imputation???       #
#--------------------------#
# need to decide on imputation method: mice?, 



# calculate customer's preferred size
# this was WAY more complicated than necessary...
# mvf = most frequent value (a.k.a mode), requires Modeest package and library
# Crap - have to make # obs match orders.sample
# also, why does this create 3 variables instead of 1?
# custMode <- summaryBy(size ~ customerID, data=orders.sample, FUN = function (x) {c(m=mfv(x))})
# custMode

# custMode <- customer

  # sorting orders by customerID to cbind customer Mode to right observation
#  r <- order(orders.sample$customerID)
#  r
#  sortID <- orders.sample[r,]
#  sortID
# cbind(sortID,custMode[,2])

# Add column to denote whether the order size was not the customer's usual order (size mode)
# had to use custMode column instead of one cbinded in. Not sure why, but this works 
# sortID$OrdNotMode <- ifelse((sortID$size != custMode[,2]),0,1)
# sortID$OrdNotMode

#     beanplot(sortID$OrdNotMode ~ returnShipment, sortID, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Unusual Size?", xaxt="n")
#     legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))

# let's try this again...
#nope
# mfv(orders.sample$size, group=orders.sample$customerID)
# mfv(orders.sample$size)

#nope
# myfun<-function(x){mfv(x)}
#  summaryBy(orders.sample$size~orders.sample$customerID, data=orders.sample, FUN=myfun)

#nope
# OB <- orderBy(~orders.sample$customerID+orders.sample$size, data=orders.sample)
#  OM <- function(d){c(NA,mfv(orders.sample$size)}
#  v<-lapplyBy(~orders.sample$customerID, data=orders.sample, OM)
#  orders.sample$OM <-unlist(v)

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

#------------#
# t-tests    #
#------------#
# We should add simple t-tests for all variables since we have a binary response variabe
# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor


# Plot Histograms for all variables by class
# will need to sub in our data names #
# I can't remember what MMST is for, but it was in a lot of my EDA code
library(MMST)

pdf(file = "hist_plots.pdf", width = 11, height = 8.5)
nm <- names(wine)[1:13]
for (i in seq(along = nm)) {
  hist.plot <- ggplot(wine,aes(x = eval(parse(text = paste("wine$", nm[i], sep=""))),
                               fill=factor(class))) + geom_histogram(alpha = 0.5)+xlab(nm[i])
  print(hist.plot)
}
dev.off()

#-------------------------#
# Density Plots by class  #
#-------------------------#
# includes a loop with output routed to a pdf file
# will need to sub in our data names #
library(ggplot2)
pdf(file = "my_plots.pdf", width = 11, height = 8.5)
nm <- names(wine)[1:13]
for (i in seq(along = nm)) {
  this.plot <- ggplot(wine,aes(x = eval(parse(text = paste("wine$", nm[i], sep=""))),
                               fill=factor(class))) + geom_density(alpha = 0.5)+xlab(nm[i])
  print(this.plot)
}
dev.off()



#------------------------------------#
# To illustrate clustering by class  #
# XY Plot by class                   #
#------------------------------------#
# lattice plots for key explanatory variables
# Shows X&Y relationship by class - Can use for EDA or after algorithm returns top vars
# But I think this may help identify interaction effects
library(lattice) # required for the xyplot() function

# this is just a template for integration #
xyplot(Flav ~ Color | class, 
       data = wine,        
       layout = c(6, 1),
       aspect=1,
       strip=function(...) strip.default(..., style=1),
       xlab = "Flavanoids", 
       ylab = "Color Intensity")

# Along same lines, we can look at scatterplots
# The larger graphs with the overlay 
# make the relationships a bit more visible
library(car)
# this is by class
scatterplot(Flav ~ Color | class, data=wine, boxplots=FALSE, 
            span=0.75, col=gray(c(0,0.5,0.7)),id.n=0)

# this is just X vs. Y.  We can adjust for any specific variable comparisons we want to look at
scatterplot(carat ~ price, data=diamonds, boxplots=FALSE, 
            span=0.75,id.n=0)

#------------------------------------------#
# Conditioned XY Plots - to look in panels #
#------------------------------------------#
# this was a handy XYplot tool to look at the relationship between 2 variables, conditioned by other variables
# this was borrowed from our diamonds data set program
# showing the relationship between price and carat, while conditioning
# on cut and channel provides a convenient view of the diamonds data
# in addition, we jitter to show all points in the data frame
xyplot(jitter(sqrtprice) ~ jitter(carat) | channel + cut, 
       data = diamonds,
       aspect = 1, 
       layout = c(3, 2),
       strip=function(...) strip.default(..., style=1),
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")


#------------------------------------------------#
# to run some Weka algorithms - good for EDA too #
#------------------------------------------------#
library(RWeka)

# May need to add pruning rules for j48 and JRip #

# to run j48 in RWeka
returns_j48 <- J48(class ~., data = orders.train)
returns_j48
summary(wine_j48)

# to add a 10-folds cross-validation (does it help?)
eval_j48 <- evaluate_Weka_classifier(returns_j48, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

# To run JRip - Recall this shows rules - will not plot a tree
returns_JRip <- JRip(class ~., data = orders.train)
returns_JRip
summary(returns_JRip)
