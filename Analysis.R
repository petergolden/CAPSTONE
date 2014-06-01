#####################################
# Run through the transformations code first to get the data with all computed variables
# The rest of this code is currently just what had been in the DMC_KT syntax after removing variable stuff
#####################################

load("~/CAPSTONE/imputedOrdersPostTransformation.rdata")

load("imputedOrdersPostTransformation.rdata")
summary(orders.train)

# eliminate cases where timeToDeliver is NA 
# removed because in training set NONE of the observations have been returned

orders.train <- orders.train[complete.cases(orders.train[,17]),]
summary(orders.train)

# Required libraries:
  # cleaned up to remove the ones that didn't appear to be used in current code
  # also moved ALL required libraries to the top here to load at once
library(beanplot)
library(doBy)
library(psych)
library(mi)
library(tseries)
library(forecast)
library(MMST)
library(ggplot2)
library(lattice) # required for the xyplot() function
library(car)
library(RWeka)
library(corrgram)


#-------------------#
#       EDA         #
#-------------------#

#----------BEAN PLOTS----------#

# Look at PDF of numeric variables given reponse
# Note that we're just using a random sample due to processing time for graphics
set.seed(498)
sample_ind <- sample(seq_len(nrow(orders.train)), size = 5000)
orders.sample <- orders.train [sample_ind, ]
str(orders.sample)
pdf("BeanPlots.pdf",width=8.5,height=11)
beanplot(customerAge ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Customer Age Distribution", ylab = "Age in Years", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(accountAge ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Account Age Distribution", ylab = "Age in Years", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(timeToDeliver ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Delivery Time Distribution", ylab = "Time in Days", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(price ~ returnShipment, orders.sample, side = "b", col = list("yellow", "orange"), border = c("yellow2","darkorange"), main = "Price Distribution", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(difFromMeanPrice ~ returnShipment, orders.sample, 
         side = "b", col = list("yellow", "orange"), 
         border = c("yellow2","darkorange"), 
         main = "Distribution of Diff From Mean Price", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(numItemsInOrder ~ returnShipment, orders.sample, 
         side = "b", col = list("yellow", "orange"), 
         border = c("yellow2","darkorange"), 
         main = "Distribution of Number of Items in Order", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(numManufOrders ~ returnShipment, orders.sample, 
         side = "b", col = list("yellow", "orange"), 
         border = c("yellow2","darkorange"), 
         main = "Distribution of Number of Times Manuf Was Ordered", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(numItemOrders ~ returnShipment, orders.sample, 
         side = "b", col = list("yellow", "orange"), 
         border = c("yellow2","darkorange"), 
         main = "Distribution of Number of Times Item Was Ordered", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
beanplot(numCustOrders ~ returnShipment, orders.sample, 
         side = "b", col = list("yellow", "orange"), 
         border = c("yellow2","darkorange"), 
         main = "Distribution of Number of Customer Orders", xaxt="n")
legend("topleft", bty="n",c("Not Returned", "Returned"), fill = c("yellow", "orange"))
dev.off()
#----------END BEAN PLOTS------#


# Mean & count of response given nominal vars
# Only doing ones with few possible values- salutation & state
summaryBy(returnShipment ~ salutation, orders.train, FUN=c(length,mean))
summaryBy(returnShipment ~ state, orders.train, FUN=c(length,mean))

# More EDA - a breakout of stats by returnShipment
describeBy(orders.train, group=orders.train$returnShipment, mat=FALSE, type=3, digits=6)


# XY Plots
# quick X vs Y plot
plot(orders.sample, cex=0.1)



#--------------------------#
# DEAL WITH MISSING VALUES #
#--------------------------#

# Not sure who added this section (JB?), but it's not working for me (KT)
# It was JB.  Maybe too much data?  Pete will pick this up.

# using mi package - get visual plot of missing obs
pdf(file = "missing_obs_plots.pdf", width = 11, height = 8.5)  ##/\open pdf/\##
missing.pattern.plot(orders.train, gray.scale = TRUE)
dev.off()										##\/close pdf\/##

# check how many observations for each variable have missing values
sum(is.na(orders.train$variable_names))


# Time-series data - taking the mean of return aggregated by order date
# NOTE- it's been awhile since I've done a TS analysis, so really I was just looking at the plots & packages here. It will likely need a fair bit of revisions.
avgReturnByDay <- summaryBy(returnShipment ~ orderDate, orders.train, FUN=mean)
ts.orders <- ts(avgReturnByDay$returnShipment.mean, start=c(2012,4), frequency=365)
pdf("TimeSeriesPlots.pdf",width=11,height=8.5)
plot(ts.orders)
acf(ts.orders,20)
pacf(ts.orders,20)
lag.plot(ts.orders,9,do.lines=F)
plot(diff(ts.orders))
acf(diff(ts.orders),20)
pacf(diff(ts.orders),20)
adf.test(ts.orders)
auto.arima(ts.orders)
dev.off()

#------------#
# t-tests    #
#------------#

# FINAL MODEL FOR VARIABLE REFERENCE
# BE.LR.Model <- glm(formula = returnShipment ~ color + timeToDeliver + salutation + 
#                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
#                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
#                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
#                     numItemOrders + numManufOrders, family = binomial(link = logit), 
#                   data = train)

# Eliminated variables: state + customerAge + holidayFlag + bdayFlag + Pants + 
#                       itemRiskFlag + numManufReturns + manufRiskFlag,

sink ("t_tests.txt")       ##/\open sink/\##
# simple t-tests for binary explanatory variables
# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor
t.test(returnShipment~holidayFlag, data=orders.train) # statistically significant
t.test(returnShipment~bdayFlag, data=orders.train) # not statistically significant
t.test(returnShipment~manufRiskFlag, data=orders.train) # Highly statistically significant
t.test(returnShipment~itemRiskFlag, data=orders.train) # Highly statistically significant
t.test(returnShipment~custRiskFlag, data=orders.train) # Highly statistically significant
t.test(returnShipment~LetterSize, data=orders.train) # statistically significant
t.test(returnShipment~Pants, data=orders.train) # not statistically significant @95% c.i.
t.test(returnShipment~ChildSize, data=orders.train) # statistically significant
t.test(returnShipment~ShoeDress, data=orders.train) # statistically significant

# We look at continuous variables as well, to see how the means of each separate
t.test(timeToDeliver~returnShipment, data=orders.train) # Statistically significant
t.test(accountAge~returnShipment, data=orders.train) # Statistically significant
t.test(difFromMeanPrice~returnShipment, data=orders.train) # Highly statistically significant
t.test(price~returnShipment, data=orders.train) # Highly statistically significant
t.test(numItemsInOrder~returnShipment, data=orders.train) # Highly statistically significant
t.test(numCustOrders~returnShipment, data=orders.train) # Highly statistically significant
t.test(numCustReturns~returnShipment, data=orders.train) # Extremely high statistical significance
t.test(numItemReturns~returnShipment, data=orders.train) # Extremely high statistical significance
t.test(numItemOrders~returnShipment, data=orders.train) # Highly statistically significant
t.test(numManufOrders~returnShipment, data=orders.train) # Highly statistically significant
t.test(customerAge~returnShipment, data=orders.train) # Highly statistically significant
t.test(numManufReturns~returnShipment, data=orders.train) # Highly statistically significant

sink()    					            ##\/close sink\/##

#---------------#
# K-S-tests 'D' #
#---------------#
# K-S-tests for continuous explanatory variables 
# K-S test won't use variables 'as-is' - need to create vectors for each response variable

sink ("KS_test_results.txt")       ##/\open sink/\##
pdf("KS_Plots.pdf",width=8.5,height=11)

# Price
cat("\n","----- K-S for Price -----","\n")
priceVector <- orders.train$price
price0 <- subset(priceVector, orders.train$returnShipment==0)
p0 <- sort(price0)
price1 <- subset(priceVector, orders.train$returnShipment==1)
p1 <- sort(price1)
ks.test(p0,p1) # statistically significant, moderate - large separation
plot(ecdf(price0), do.points=FALSE, verticals=T, xlab="price", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(price1), lty=3, do.points=FALSE, verticals=T)
remove(price0, price1, priceVector, p0, p1) #clean workspace

# Delivery Time
cat("\n","----- K-S for Delivery Time -----","\n")
# Testng to see if K-S function ranks for us, or if we have to do manually
shipTime <- orders.train$timeToDeliver
ship0 <- subset(shipTime, orders.train$returnShipment==0)
ship1 <- subset(shipTime, orders.train$returnShipment==1)
ks.test(ship0,ship1) # statistically significant, very minor separation
plot(ecdf(ship0), do.points=FALSE, verticals=T, xlab="Delivery Time", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(ship1), lty=3, do.points=FALSE, verticals=T)
remove(ship0, ship1, shipTime) #clean workspace

# Looks like we don't have to rank vectors, ks.test does that for us - yay!
# shipTime <- orders.train$timeToDeliver
# ship0 <- subset(shipTime, orders.train$returnShipment==0)
# s0 <- sort(ship0)
# ship1 <- subset(shipTime, orders.train$returnShipment==1)
# s1 <- sort(ship1)
# ks.test(s0,s1) # statistically significant, minor separation
# remove(ship0, ship1, shipTime, s0, s1) #clean workspace

# Age of Account
cat("\n","----- K-S for Age of Account -----","\n")
acctAge <- orders.train$accountAge
acctAge0 <- subset(acctAge, orders.train$returnShipment==0)
acctAge1 <- subset(acctAge, orders.train$returnShipment==1)
ks.test(acctAge0, acctAge1) # statistically significant, very little separation, though
plot(ecdf(acctAge0), do.points=FALSE, verticals=T, xlab="Age of Account", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(acctAge1), lty=3, do.points=FALSE, verticals=T)
remove(acctAge0, acctAge1, acctAge) #clean workspace

# Age of Customer
cat("\n","----- K-S for Age of Customer -----","\n")
custAge <- orders.train$customerAge
custAge0 <- subset(custAge, orders.train$returnShipment==0)
custAge1 <- subset(custAge, orders.train$returnShipment==1)
ks.test(custAge0, custAge1) # statistically significant, minor separation
plot(ecdf(custAge0), do.points=FALSE, verticals=T, xlab="Age of Customer", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(custAge1), lty=3, do.points=FALSE, verticals=T)
remove(custAge0, custAge1, custAge) #clean workspace

# Number of Items in that day's order - proxy for basket size
cat("\n","----- K-S for Basket Size Proxy -----","\n")
numItems <- orders.train$numItemsInOrder
numItems0 <- subset(numItems, orders.train$returnShipment==0)
numItems1 <- subset(numItems, orders.train$returnShipment==1)
ks.test(numItems0, numItems1) # statistically significant, moderate - large separation
plot(ecdf(numItems0), do.points=FALSE, verticals=T, xlab="Number of Items in 'Basket'", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(numItems1), lty=3, do.points=FALSE, verticals=T)
remove(numItems0, numItems1, custAge) #clean workspace

# Frequency of returns for that manufacturer
cat("\n","----- K-S for Manufacturer Returns -----","\n")
manRet <- orders.train$numManufReturns
manRet0 <- subset(manRet, orders.train$returnShipment==0)
manRet1 <- subset(manRet, orders.train$returnShipment==1)
ks.test(manRet0, manRet1) # statistically significant, moderate - large separation
plot(ecdf(manRet0), do.points=FALSE, verticals=T, xlab="Return Frequency for Item Manufacturer", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(manRet1), lty=3, do.points=FALSE, verticals=T)
remove(manRet0, manRet1, manRet) #clean workspace

# Number of orders for a manufacturer
cat("\n","----- K-S for Manufacturer Orders -----","\n")
manOrd <- orders.train$numManufOrders
manOrd0 <- subset(manOrd, orders.train$returnShipment==0)
manOrd1 <- subset(manOrd, orders.train$returnShipment==1)
ks.test(manOrd0, manOrd1) # statistically significant, minor separation
plot(ecdf(manOrd0), do.points=FALSE, verticals=T, xlab="Number of Items Sold for that Manufacturer", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(manOrd1), lty=3, do.points=FALSE, verticals=T)
remove(manOrd0, manOrd1, manOrd) #clean workspace

# Frequency of returns for item ordered
cat("\n","----- K-S for Item Return Frequency -----","\n")
itemRet <- orders.train$numItemReturns
itemRet0 <- subset(itemRet, orders.train$returnShipment==0)
itemRet1 <- subset(itemRet, orders.train$returnShipment==1)
ks.test(itemRet0, itemRet1) # statistically significant, VERY large separation
plot(ecdf(itemRet0), do.points=FALSE, verticals=T, xlab="Return Frequency for Item", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(itemRet1), lty=3, do.points=FALSE, verticals=T)
remove(itemRet0, itemRet1, itemRet) #clean workspace

# Number of orders for item ordered
cat("\n","----- K-S for Total Number Sold of Item -----","\n")
itemOrd <- orders.train$numItemOrders
itemOrd0 <- subset(itemOrd, orders.train$returnShipment==0)
itemOrd1 <- subset(itemOrd, orders.train$returnShipment==1)
ks.test(itemOrd0, itemOrd1) # statistically significant, minor - moderate separation
plot(ecdf(itemOrd0), do.points=FALSE, verticals=T, xlab="Number of Items Sold for that ItemID", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(itemOrd1), lty=3, do.points=FALSE, verticals=T)
remove(itemOrd0, itemOrd1, itemOrd) #clean workspace

# Difference from the Mean Price for that item
cat("\n","----- K-S for Price Less Mean of Price -----","\n")
meanDif <- orders.train$difFromMeanPrice
meanDif0 <- subset(meanDif, orders.train$returnShipment==0)
meanDif1 <- subset(meanDif, orders.train$returnShipment==1)
ks.test(meanDif0, meanDif1) # statistically significant, minor - moderate separation
plot(ecdf(meanDif0), do.points=FALSE, verticals=T, xlab="Difference from Item Price Mean", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(meanDif1), lty=3, do.points=FALSE, verticals=T)
remove(meanDif0, meanDif1, meanDif) #clean workspace

# Frequency of returns for that Customer
cat("\n","----- K-S for Return Frequency for Customer -----","\n")
custRet <- orders.train$numCustReturns
custRet0 <- subset(custRet, orders.train$returnShipment==0)
custRet1 <- subset(custRet, orders.train$returnShipment==1)
ks.test(custRet0, custRet1) # statistically significant, GARGANTUAN SEPARATION
plot(ecdf(custRet0), do.points=FALSE, verticals=T, xlab="Return Frequency for Customer", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(custRet1), lty=3, do.points=FALSE, verticals=T)
remove(custRet0, custRet1, custRet) #clean workspace

# Number of orders for that Customer
cat("\n","----- K-S for Total Orders for Customer -----","\n")
custOrd <- orders.train$numCustOrders
custOrd0 <- subset(custOrd, orders.train$returnShipment==0)
custOrd1 <- subset(custOrd, orders.train$returnShipment==1)
ks.test(custOrd0, custOrd1) # statistically significant, minor separation
plot(ecdf(custOrd0), do.points=FALSE, verticals=T, xlab="Total Number of Items Purchased for given Customer", ylab="cumulative distribution", main="K-S Plot")
lines(ecdf(custOrd1), lty=3, do.points=FALSE, verticals=T)
remove(custOrd0, custOrd1, custOrd) #clean workspace

dev.off()
sink()      				            ##\/close sink\/##


#------------End KS----------------#

#------------------------#
#     Correlation        #
#------------------------#

library(Hmisc)

sink ("correlations.txt")   	  ##/\open sink/\##
# included in LR BE
cat("\n","----- correlation and significance therein between returns and color -----","\n")
rcorr(orders.train$returnShipment, orders.train$color, type="pearson")

cat("\n","----- correlation and significance therein between returns and color -----","\n")
rcorr(orders.train$returnShipment, orders.train$timeToDeliver, type="pearson")

cat("\n","----- correlation and significance therein between returns and salutation -----","\n")
rcorr(orders.train$returnShipment, orders.train$salutation, type="pearson")

cat("\n","----- correlation and significance therein between returns and account age -----","\n")
rcorr(orders.train$returnShipment, orders.train$accountAge, type="pearson")

cat("\n","----- correlation and significance therein between returns and Christmas time -----","\n")
rcorr(orders.train$returnShipment, orders.train$holidayFlag, type="pearson")

cat("\n","----- correlation and significance therein between returns and sizing in letters -----","\n")
rcorr(orders.train$returnShipment, orders.train$LetterSize, type="pearson")

cat("\n","----- correlation and significance therein between returns and childrens' sizes -----","\n")
rcorr(orders.train$returnShipment, orders.train$ChildSize, type="pearson")

cat("\n","----- correlation and significance therein between returns and size range for shoes and dresses -----","\n")
rcorr(orders.train$returnShipment, orders.train$ShoeDress, type="pearson")

cat("\n","----- correlation and significance therein between returns and high risk sizes -----","\n")
rcorr(orders.train$returnShipment, orders.train$sizeHighRisk, type="pearson")

cat("\n","----- correlation and significance therein between returns and low risk sizes -----","\n")
rcorr(orders.train$returnShipment, orders.train$sizeLowRisk, type="pearson")

cat("\n","----- correlation and significance therein between returns and difference between transaction price and mean -----","\n")
rcorr(orders.train$returnShipment, orders.train$difFromMeanPrice, type="pearson")

cat("\n","----- correlation and significance therein between returns and price -----","\n")
rcorr(orders.train$returnShipment, orders.train$price, type="pearson")

cat("\n","----- correlation and significance therein between returns and basket size proxy -----","\n")
rcorr(orders.train$returnShipment, orders.train$numItemsInOrder, type="pearson")

cat("\n","----- correlation and significance therein between returns and total number of customer orders -----","\n")
rcorr(orders.train$returnShipment, orders.train$numCustOrders, type="pearson")

cat("\n","----- correlation and significance therein between returns and return frequency by customer -----","\n")
rcorr(orders.train$returnShipment, orders.train$numCustReturns, type="pearson")

cat("\n","----- correlation and significance therein between returns and indicator for a high risk customer -----","\n")
rcorr(orders.train$returnShipment, orders.train$custRiskFlag, type="pearson")

cat("\n","----- correlation and significance therein between returns and total number of returns for an item -----","\n")
rcorr(orders.train$returnShipment, orders.train$numItemReturns, type="pearson")

cat("\n","----- correlation and significance therein between returns and total number of orders for that item -----","\n")
rcorr(orders.train$returnShipment, orders.train$numItemOrders, type="pearson")

cat("\n","----- correlation and significance therein between returns and the number of orders for that manufacturer -----","\n")
rcorr(orders.train$returnShipment, orders.train$numManufOrders, type="pearson")

sink()  						            ##\/close sink\/##


# correl includes the undelivereds - check
length(orders.train$timeToDeliver)
summary(orders.train$timeToDeliver)
length(orders.train$returnShipment)
summary(orders.train$returnShipment)

# excluded from LR BE
rcorr(orders.train$returnShipment, orders.train$itemRiskFlag, type="pearson")
rcorr(orders.train$returnShipment, orders.train$manufRiskFlag, type="pearson")
rcorr(orders.train$returnShipment, orders.train$bdayFlag, type="pearson") # Not statistically significant
rcorr(orders.train$returnShipment, orders.train$Pants, type="pearson")
rcorr(orders.train$returnShipment, orders.train$LetterSize, type="pearson")
rcorr(orders.train$returnShipment, orders.train$state, type="pearson") # Not statistically significant


cor(orders.train$returnShipment, orders.train$timeToDeliver)

# Plot Histograms for all variables by class
# will need to sub in our data names #

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
pdf(file = "density_plots.pdf", width = 11, height = 8.5)
nm <- names(orders.train)[1:55]
for (i in seq(along = nm)) {
  this.plot <- ggplot(orders.train,aes(x = eval(parse(text = paste("returnShipment$", nm[i], sep=""))),
                               fill=factor(class))) + geom_density(alpha = 0.5)+xlab(nm[i])
  print(this.plot)
}
dev.off()


# NOT working
pdf(file = "qplots_density.pdf", width = 11, height = 8.5)
qplot(orders.train$price, data=orders.train, geom="density", fill=orders.train$returnShipment, alpha=I(.5),
      main="price by return status", xlab="price",
      ylab="Density")
dev.off()


#------------------------------------#
# To illustrate clustering by class  #
# XY Plot by class                   #
#------------------------------------#
# lattice plots for key explanatory variables
# Shows X&Y relationship by class - Can use for EDA or after algorithm returns top vars
# But I think this may help identify interaction effects
library(lattice) # required for the xyplot() function

plot(, )

# this is just a template for integration #
xyplot(numManufReturns ~ numCustReturns | returnShipment, 
       data = orders.train,        
       layout = c(6, 1),
       aspect=1,
       strip=function(...) strip.default(..., style=1),
xlab = "Number of returns by Customer", 
ylab = "Number of returns by Manufacturer")

# Along same lines, we can look at scatterplots - need car library
library(car)
# The larger graphs with the overlay 
# make the relationships a bit more visible
# this is by class
# boxes are too small; size adjustment?
scatterplot(numManufReturns ~ numCustReturns | returnShipment, data=orders.train, boxplots=FALSE, 
            span=0.75, col=gray(c(0,0.5,0.7)),id.n=0)

# this is just X vs. Y.  We can adjust for any specific variable comparisons we want to look at
scatterplot(numManufReturns ~ numCustReturns, data=orders.train, boxplots=FALSE, 
            span=0.75,id.n=0)



# Numeric fields for sample scatterplot
orders.numeric <- orders.sample[c("price","timeToDeliver","accountAge","customerAge","numItemOrders","numManufOrders","numCustOrders","numItemsInOrder","difFromMeanPrice")]
# (KT) I've been using this version for my scatterplot matrix
panel.cor <-function(x,y,digits=3,prefix="",cex.cor,...){ # gives you the ability to show correlation coefficients in the matrix
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r,0.123456789),digits=digits)[1]
  txt <- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  cex.col <- "black"
  if(r > 0.4) cex.col="blue" # this highlights large values- if not needed you can remove this line and the one above, as well as the col= below
  text(0.5,0.5,txt,cex = cex.cor*(1+r)/2,col=cex.col)
} # Modified from R Graphics Cookbook
panel.density <-function(x,...){ # allows you to show density on the diagonal
  usr <- par("usr")
  on.exit(par(usr))
  dd <- density(x, na.rm=TRUE)
  xr = range(dd$x)
  yr = range(dd$y)
  par(usr = c(min(xr), max(xr), min(yr), max(yr) * 1.5))
  plot.xy(xy.coords(dd$x, dd$y), type = "l", col = "black", 
          ...)
  box(col = "lightgray")
} # Modified from corrgram package version of panel.density

pairs(orders.numeric,pch=".",diag.panel=panel.density,lower.panel=panel.smooth,upper.panel=panel.cor,main="Scatterplot Matrix") 
# pch = "." uses dots instead of circles for points. Leave out if circles are what you want.

panel.correl <- function (x, y, corr = NULL, col.regions, digits = 2, cex.cor, 
          ...) 
{
  auto <- missing(cex.cor)
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  if (!is.null(corr)) {
    est <- corr
    est <- formatC(est, digits = digits, format = "f")
    if (auto) 
      cex.cor <- 0.7/strwidth(est)
    cex.col <- "black"
    if(est > 0.2) cex.col="blue" # this highlights large values- if not needed you can remove this line and the one above, as well as the col= below
    text(0.5, 0.6, est, cex = cex.cor, col=cex.col)
  }
  else {
    results <- cor.test(x, y, alternative = "two.sided")
    est <- results$estimate
    est <- formatC(est, digits = digits, format = "f")
    if (auto) 
      cex.cor <- 0.7/strwidth(est)
    cex.col <- "black"
    if(abs(as.numeric(est)) > 0.2) cex.col="red"
    if(est > 0.2) cex.col="blue" # this highlights large values- if not needed you can remove this line and the one above, as well as the col= below
    text(0.5, 0.6, est, cex = cex.cor,col=cex.col)
  }
}
# (KT) Alternate using corrgram package
pdf("EDA_Correlations.pdf",width=11,height=8.5)
corrgram(orders.numeric,main="Correlations",
         lower.panel=panel.ellipse,
         upper.panel=panel.correl,
         diag.panel=panel.density)
dev.off()

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

# May need to add pruning rules for j48 and JRip #

# To run JRip - Recall this shows rules - will not plot a tree
returns_JRip <- JRip(class ~., data = orders.train)
returns_JRip
summary(returns_JRip)
