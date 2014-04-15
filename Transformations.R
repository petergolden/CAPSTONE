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
custMode1 <- summaryBy(toupper(as.character(orders.train$size)) ~ customerID, orders.train, FUN=mymode)
custMode2 <- summaryBy(orders.train$sizeLetter ~ customerID, orders.train, FUN=mymode)
custMode3 <- summaryBy(orders.train$sizePant ~ customerID, orders.train, FUN=mymode)
custMode4 <- summaryBy(orders.train$sizeChild ~ customerID, orders.train, FUN=mymode)
custMode5 <- summaryBy(orders.train$sizeOther ~ customerID, orders.train, FUN=mymode)
custMode <- merge(custMode1,custMode2,by="customerID")
custMode <- merge(custMode,custMode3,by="customerID")
custMode <- merge(custMode,custMode4,by="customerID")
custMode <- merge(custMode,custMode5,by="customerID")

# -------------------------------------------- #
# Ideas for other variables
# -------------------------------------------- #
# breakout of sizes - done, but may revisit to look at tying to items?
# mode for all size variables by customer - started, need to troubleshoot code
# number of items per order
# flag for if an item's price drops within x number of days of purchase
# 