# Variable Transformations
# Libraries
library(doBy)

# Read in data from GitHub
orders.train <- read.table("orders_train.txt", header = TRUE, sep = ";")
str(orders.train)

# Update date fields to date type instead of factors
orders.train$orderDate <- as.Date(orders.train$orderDate, format = "%Y-%m-%d")
orders.train$deliveryDate <- as.Date(orders.train$deliveryDate, format = "%Y-%m-%d")
orders.train$dateOfBirth <- as.Date(orders.train$dateOfBirth, format = "%Y-%m-%d")
orders.train$creationDate <- as.Date(orders.train$creationDate, format = "%Y-%m-%d")

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

# Sizing recodes - creating a table with frequencies to work from and going to remove sizes as I recode them
# There may be some errors here- for example, Euro children's sizes start at 50, but some conversions go up to size 52 for men's suits, etc
size.table <- summaryBy(size ~ size,orders.train,FUN=length)
View(size.table)
# Ones that seem like US sizes
orders.train$sizeLetter <- ifelse(as.character(orders.train$size)>"a",toupper(as.character(orders.train$size)),NA)
size.table <- size.table[-which(as.character(size.table$size)>"a"),]
orders.train$sizePant <- ifelse(as.numeric(as.character(orders.train$size))>2900,as.numeric(as.character(orders.train$size)),NA)
size.table <- size.table[-which(as.numeric(as.character(size.table$size))>2900),]
# Euro children's sizes go from 50-188
# http://www.ebay.com/gds/Guide-to-Understanding-European-Clothing-Sizes-/10000000007740616/g.html
orders.train$sizeChild <- ifelse(as.numeric(as.character(orders.train$size))>=50 & as.numeric(as.character(orders.train$size))<=188,as.numeric(as.character(orders.train$size)),NA)
size.table <- size.table[-which(as.numeric(as.character(size.table$size))>=50 & as.numeric(as.character(size.table$size))<=188),]
# Remaining
orders.train$sizeOther <- ifelse(is.na(orders.train$sizeLetter) & is.na(orders.train$sizePant) & is.na(orders.train$sizeChild),as.character(orders.train$size),NA)

# check
table(orders.train$sizeLetter)
table(orders.train$sizePant)
table(orders.train$sizeChild)
table(orders.train$sizeOther)

# Add mode function - note that this only gives one mode if there is more than one
mymode <- function(x){
  names(sort(-table(as.character(x))))[1]
}
custMode1 <- summaryBy(toupper(as.character(orders.train$size)) ~ orders.train$customerID, orders.train, FUN=mymode)
custMode2 <- summaryBy(sizeLetter ~ customerID, orders.train[-which(is.na(orders.train$sizeLetter)),], FUN=mymode)
custMode3 <- summaryBy(sizePant ~ customerID, orders.train[-which(is.na(orders.train$sizePant)),], FUN=mymode)
custMode4 <- summaryBy(sizeChild ~ customerID, orders.train[-which(is.na(orders.train$sizeChild)),], FUN=mymode)
custMode5 <- summaryBy(sizeOther ~ customerID, orders.train[-which(is.na(orders.train$sizeOther)),], FUN=mymode)
custMode <- merge(custMode1,custMode2,by="customerID",all=T)
custMode <- merge(custMode,custMode3,by="customerID",all=T)
custMode <- merge(custMode,custMode4,by="customerID",all=T)
custMode <- merge(custMode,custMode5,by="customerID",all=T)
names(custMode) <- c("customerID","sizeMode","szLetterMode","szPantMode", "szChildMode", "szOtherMode")
# Merge back into original file, then drop the unnecessary data frames to clean up the workspace
orders.train <- merge(orders.train,custMode,by="customerID",all=T)
remove(custMode,custMode1,custMode2,custMode3,custMode4,custMode5,size.table)

# Add holiday/bday flags
orders.train$holidayFlag <- ifelse(as.character(orders.train$orderDate,format="%m%d")>="1125" &
  as.character(orders.train$orderDate,format="%m%d")<="1230",1,0)
orders.train$bdayFlag <- ifelse(as.character(orders.train$orderDate,format="%m%d")>=as.character(orders.train$dateOfBirth-30,format="%m%d") & 
  as.character(orders.train$orderDate,format="%m%d")<=as.character(orders.train$dateOfBirth+5,format="%m%d"),1,0)
# Need to address cases at the beginning/end of the year
# Get count of bdayFlags for comparison in next step
summaryBy(bdayFlag ~ 1, orders.train, FUN=sum, na.rm=T)
# Bdays after 12/26 or before 1/31 - the order date can be until the end of the year and part of the next
orders.train$bdayFlag <- ifelse((as.character(orders.train$dateOfBirth,format="%m%d")>"1226"|as.character(orders.train$dateOfBirth,format="%m%d")<="0130") & 
  (as.character(orders.train$orderDate,format="%m%d")>=as.character(orders.train$dateOfBirth-30,format="%m%d") | as.numeric(as.character(orders.train$orderDate,format="%m%d"))<=as.numeric(as.character(orders.train$dateOfBirth+5,format="%m%d"))),1,orders.train$bdayFlag)
summaryBy(bdayFlag ~ 1, orders.train, FUN=sum, na.rm=T)
# Visually check some of these - appears to be working correctly
View(orders.train[which(orders.train$bdayFlag==1 & (as.character(orders.train$orderDate,format="%m%d")<"0104" | as.character(orders.train$orderDate,format="%m%d")>"1215")),])

# Add number of items per order
numItems <- summaryBy(orderItemID ~ customerID + orderDate, orders.train, FUN=length)
names(numItems) <- c("customerID","orderDate","numItemsInOrder")
orders.train <- merge(orders.train,numItems,by=c("customerID","orderDate"))
# Add num items with that items ID per order
# Also looking at number of returns since we expect higher returns if they order dups
dupItems <- summaryBy(returnShipment ~ customerID + orderDate + itemID, orders.train, FUN=c(length,sum))
names(dupItems) <- c("customerID","orderDate","itemID","numItemID","numItemIDReturned")
# Check hypothesis
summaryBy(numItemIDReturned ~ numItemID, dupItems, FUN=c(length,median,mean))
# Leaving out number returned when merging back in because we don't want to accidentally include as a predictor
orders.train <- merge(orders.train,dupItems[1:4],by=c("customerID","orderDate","itemID"))
# Dropping unnecessary data frames
remove(numItems,dupItems)

# Find high risk manufacturers/items/customers
# Do we want to only include ones that had a certain number of items ordered? 
# I picked 50 as a cutoff, but that's arbitrary; could use median or a percentile
    # Manufacturers
riskyManuf <- summaryBy(returnShipment ~ manufacturerID,orders.train,FUN=c(length,mean))
summary(riskyManuf$returnShipment.mean)
summary(riskyManuf$returnShipment.length)
summary(riskyManuf[which(riskyManuf$returnShipment.length>=50),]$returnShipment.mean)
# Using top quartile for a risk cutoff
riskyManuf$manufRiskFlag <- ifelse(riskyManuf$returnShipment.length>=50 & riskyManuf$returnShipment.mean >=0.5573,1,0)
names(riskyManuf) <- c("manufacturerID","numManufOrders","numManufReturns","manufRiskFlag")
# Merge
orders.train <- merge(orders.train,riskyManuf,by="manufacturerID")
    # Items
riskyItems <- summaryBy(returnShipment ~ itemID,orders.train,FUN=c(length,mean))
summary(riskyItems$returnShipment.mean)
summary(riskyItems$returnShipment.length)
summary(riskyItems[which(riskyItems$returnShipment.length>=50),]$returnShipment.mean)
# Using top quartile for a risk cutoff
riskyItems$itemRiskFlag <- ifelse(riskyItems$returnShipment.length>=50 & riskyItems$returnShipment.mean >=0.5938,1,0)
names(riskyItems) <- c("itemID","numItemOrders","numItemReturns","itemRiskFlag")
# Merge
orders.train <- merge(orders.train,riskyItems,by="itemID")
    # Customers
riskyCust <- summaryBy(returnShipment ~ customerID,orders.train,FUN=c(length,mean))
summary(riskyCust$returnShipment.mean)
summary(riskyCust$returnShipment.length)
summary(riskyCust[which(riskyCust$returnShipment.length>=50),]$returnShipment.mean)
# Using top quartile for a risk cutoff
riskyCust$custRiskFlag <- ifelse(riskyCust$returnShipment.length>=50 & riskyCust$returnShipment.mean >=0.6667,1,0)
names(riskyCust) <- c("customerID","numCustOrders","numCustReturns","custRiskFlag")
# Merge & clear workspace
orders.train <- merge(orders.train,riskyCust,by="customerID")
remove(riskyManuf,riskyItems,riskyCust)

# Check if items are always the same price (expect they're not, but wanted to verify before coding more)
# Using a merge because if I try to do quantile and mean in 1 step, the labels aren't clear
itemPricing <- merge(summaryBy(price ~ itemID,orders.train,FUN=quantile),
    summaryBy(price ~ itemID,orders.train,FUN=mean),by="itemID")
View(itemPricing) # confirmed, going to attach to the orders.train data frame so we can later flag 
orders.train <- merge(orders.train, itemPricing, by="itemID")
remove(itemPricing)
# Look at mean of returnShipment for each price point
# Currently saving this out as a separate table because I'm not entirely sure what to do with it
returnsByPrice <- summaryBy(returnShipment ~ itemID + price, orders.train, FUN=c(length,mean))
                            
# -------------------------------------------- #
# Ideas for other variables
# -------------------------------------------- #
#
# General customer behavior principles (hypotheses) #
#
  # people know their own size the best
  # people know their own sizing system best
  # some articles of clothing are more difficult to size than others (i.e. pants > t-shirts)
  # people are generally motivated to save money (price sensitivity)
    # Some customers are MORE price sensitive
  # people generally want an item when it is fashionable (fashion sensitivity)
    # Some customers are MORE fashion sensitive
  # A purchase outside a customer’s profile is more likely to be returned
#
# Tasks #
#
# We may consider sorting these later by types of risks 
    # e.g. customer risk, manufacturer risk, price risk, etc.
#
# breakout of sizes - done, but may revisit to look at tying to items?
# mode for all size variables by customer - done
# number of items per order - done
# flag for if an item's price drops within x number of days of purchase
#

#### I'm not entirely sure how to code for these two
# flag for if customer is price sensitive 
    # may yield other interactions, like increased propensity to return if price drop)
# flag for if customer is fashion sensitive (may order at earlier dates)
    # may yield other interactions, like increased propensity to return if product is older or cheaper
#
# colorDuplicate = same item ordered on orderDate but in different color as well 
    # instead of doing this, I JUST flagged when the same itemID was ordered more than once
# orderDuplicate = same exact item ordered >1x on orderDate
    # see above
# holidayOrder = orderDate or deliveryDate is within 30 days prior or 5 days post Xmas- done
# birthdayOrder = order is within 30 days prior or 5 days post customer’s birthday - done
    # (may have to work a way to make it check bday each year)

# I did the high risk stuff for customer, item, and manufacturer. It could be done for size and color pretty easily, but that didn't make as much sense to me.

# highRiskCustomer = has the customer returned greater than X% of items? 
# (something above the mean return rate)
# highRiskManufacturer = does this manufacturer get a high % of returns 
    # (may be cleaner than listing out or creating nodes on each manufacturerID)
# highRiskColor = does this color get returned more frequently 
    # (may be cleaner than listing out or creating nodes on each color)
# highRiskItem = does this item get returned more frequently 
    # (may be cleaner than listing out or creating nodes on each itemID)
# Bought on sale?  Or bought at >X% discount?  (each item has a maximum price) 
    # we could see if different price points for each item results in a 
    # different return rate. This would be separate from the subsequent sale returns
        # I mapped in max and min price for the item but haven't really done anything else with it... do we want to flag if it's the max price, min price, % discount...?
# UK or US Manufacturer (indicator variable - based on sizing conventions)

