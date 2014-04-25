#####################################
# Run through the transformations code first to get the data with all computed variables
# The rest of this code is currently just what had been in the DMC_KT syntax after removing variable stuff
#####################################

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

# Train/Test split (doing 70/30, based on number of orders)
# Just writing out syntax here- probably makes more sense to put it after the EDA, though.
# There's probably a more elegant way to do this but I just went with syntax I already know. Feel free to update.
smp_size <- floor(0.7 * max(orders.train$orderID))
set.seed(498)
train_ind <- sample(seq_len(max(orders.train$orderID)), size = smp_size)
orders.train$trainTest <- train_ind[orders.train$orderID]
train <- orders.train[which(orders.train$trainTest>0), ]
test <- orders.train[-which(orders.train$trainTest>0), ]
remove(smp_size,train_ind)

# Look at PDF of numeric variables given reponse
# Note that we're just using a random sample due to processing time for graphics
set.seed(498)
sample_ind <- sample(seq_len(nrow(orders.train)), size = 100)
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

# Not sure who added this section (JB?), but it's not working for me (KT)

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
# We should add simple t-tests for binary variables since we have a binary response variabe
# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor


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
# this is by class
scatterplot(Flav ~ Color | class, data=wine, boxplots=FALSE, 
            span=0.75, col=gray(c(0,0.5,0.7)),id.n=0)

# this is just X vs. Y.  We can adjust for any specific variable comparisons we want to look at
scatterplot(carat ~ price, data=diamonds, boxplots=FALSE, 
            span=0.75,id.n=0)

# Numeric fields for sample scatterplot
orders.numeric <- orders.sample[c("price","timeToDeliver","accountAge","customerAge","numItemOrders","numItemID")]
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

pairs(Orders.numeric,pch=".",diag.panel=panel.density,lower.panel=panel.smooth,upper.panel=panel.cor,main="Scatterplot Matrix") 
# pch = "." uses dots instead of circles for points. Leave out if circles are what you want.

# (KT) Alternate using corrgram package
corrgram(orders.numeric,main="Correlations",lower.panel=panel.ellipse,diag.panel=panel.density)

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
