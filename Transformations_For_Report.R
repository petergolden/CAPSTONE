# Variable Transformations
# Libraries
library(doBy)
library(ggplot2)
library(mice)
library(plyr)
# Read in data from GitHub
orders.train <- read.table("orders_train.txt", header = TRUE, sep = ";")
orders.class <- read.table("orders_class.txt", header = TRUE, sep = "\t")

orders.train <- orders.class
#Read in our color mapping
colorMap <- read.table("Color mapping.csv", header = TRUE, sep = ",", as.is = TRUE)


#-----------------------#
# Correct Data Formats  #
#-----------------------#

# Update date fields to date type instead of factors
orders.train$orderDate <- as.Date(orders.train$orderDate, format = "%Y-%m-%d")
orders.train$deliveryDate <- as.Date(orders.train$deliveryDate, format = "%Y-%m-%d")
orders.train$dateOfBirth <- as.Date(orders.train$dateOfBirth, format = "%Y-%m-%d")
orders.train$creationDate <- as.Date(orders.train$creationDate, format = "%Y-%m-%d")

# The class file has a different format, so use the following lines
orders.train$orderDate <- as.Date(orders.train$orderDate, format = "%m/%d/%Y")
orders.train$deliveryDate <- as.Date(orders.train$deliveryDate, format = "%m/%d/%Y")
orders.train$dateOfBirth <- as.Date(orders.train$dateOfBirth, format = "%m/%d/%Y")
orders.train$creationDate <- as.Date(orders.train$creationDate, format = "%m/%d/%Y")


#--------------------#
# QA Report Output   #
#--------------------#

# Summary data for the QA report
summary(orders.train$orderDate)
summary(orders.train$deliveryDate)
summary(orders.train$creationDate)
summary(orders.train$dateOfBirth)

# Export top and bottom 10 cases for QA report
print(orders.train[1:10,])
print(orders.train[481083:481092,])

# Export frequency tables for categorical variables (not IDs)
table(orders.train$size)
table(orders.train$color)
table(orders.train$salutation)
table(orders.train$state)
table(orders.train$returnShipment)

# Bar plots
par(las=2) # make label text perpendicular to axis
options(scipen=50,digits=5) # change default setting for when R converts to using sci notation
barplot(table(orders.train$size), main="Size Distribution", horiz=TRUE, cex.names=0.5)
par(mar=c(4,6,4,2)) # increase y-axis margin (order is bottom,left,top,right)
barplot(table(orders.train$color), main="Color Distribution", horiz=TRUE, cex.names=0.6)
barplot(table(orders.train$salutation), main="Salutation Distribution", horiz=TRUE)
par(mar=c(4,10,4,2)) # increase y-axis margin (order is bottom,left,top,right)
barplot(table(orders.train$state), main="State Distribution", horiz=TRUE, cex.names=0.6)

#-----------------------#
# End QA Report output  #
#-----------------------#

#---------------#
# Data Checks   #
#---------------#

# Check that orderItemID is a uniqueID
# Since it looks like it's just the record number, it should match the row number
# Printing cases where it's not- this should return 0 records
orders.train[-which(orders.train$orderItemID!=orders.train$row.names),]

# Check that each itemID is associated with just one manufacturerID
# Create a table by item ID with the min & max of manufacturer ID
item.check <- summaryBy(manufacturerID ~ itemID, orders.train, FUN=c(min,max))
# then check that there are no cases where the min and max are different
item.check[-which(item.check$manufacturerID.min==item.check$manufacturerID.max),]
# get full details on these itemIDs
View(orders.train[which(orders.train$itemID==c(1627,1682,1696,2252)),])

#Get the mean of the returns and number of returns out of the 145 obs
X <- orders.train[which(orders.train$itemID==c(1627,1682,1696,2252)),]
mean(X$returnShipment) #.5034483
sum(X$returnShipment) # 73

# The following binomial test on these 'missing' obs show mean value of these; 
#check if within the confidence interval for no bias
binom.test(73, 145, p = .4824, alternative = c("two.sided"), conf.level = 0.95)
#x= number of 'successes',n=number of trials, 
#p = hypothesized prob of success, or that of return rate of our population
#Refer to http://stat.ethz.ch/R-manual/R-patched/library/stats/html/binom.test.html

#t-test on equal means
t.test(X$returnShipment, orders.train$returnShipment)
# get a high p-value ( p-value = 0.6147) so we cannot reject null hypothesis of equal means
# Since true prob is in confidence interval, there is no bias and should remove these observations
remove(item.check, X)

# Customer checks- salutation, state, bday, and creation date should match across records
# Steps similar to item check above
cust.check <- summaryBy(salutation + state + dateOfBirth + creationDate ~ customerID, orders.train, FUN=c(min,max))
cust.check[-which(cust.check$salutation.min==cust.check$salutation.max),] # 0 records
cust.check[-which(cust.check$state.min==cust.check$state.max),] # 0 records
cust.check[-which(cust.check$dateOfBirth.min==cust.check$dateOfBirth.max),] # large number of records, viewing the table instead
View(cust.check[-which(cust.check$dateOfBirth.min==cust.check$dateOfBirth.max),]) #\All NAs, which is ok
cust.check[-which(cust.check$creationDate.min==cust.check$creationDate.max),] # 0 records
remove(cust.check)

#--------------------------#
# Variable Transformations #
#     And Data Cleaning    #
#--------------------------#


summary(orders.train)
#NA's in columns 3,11

#Look for bias in observations with missing data
# check missing delivery date
missing3  <- orders.train[(is.na(orders.train$deliveryDate)),]
head(missing3)
summary(missing3) # 39419 obs
sum(missing3$returnShipment) # 0 returns confirmed

#Check missing date of birth
missing11 <- orders.train[(is.na(orders.train$dateOfBirth)),]
head(missing11)
summary(missing11) # 48889 obs
sum(missing11$returnShipment) # 23290 returns 
mean(missing11$returnShipment) #0.4763853
binom.test(23290, 48889, p = .4824, alternative = c("two.sided"), conf.level = 0.95) # p-value = 0.007791
t.test(missing11$returnShipment, orders.train$returnShipment) # p-value = 0.01085
# both results are outside confidence interval with significant p values 
# reject null of equal means - so there is potential for a slight bias, 
# Exact tests for data sets this large are very sensitive, this is not a major concern

#Color
missing5 <- orders.train[(is.na(orders.train$color)),] #143 obs
sum(missing5$returnShipment) # 7 returns 
mean(missing5$returnShipment) # 0.04895105
binom.test(7, 143, p = .4824, alternative = c("two.sided"), conf.level = 0.95) # p-value < 2.2e-16
t.test(missing5$returnShipment, orders.train$returnShipment) # p-value < 2.2e-16

#Salutation - we called not reported as 'missing'
missing10 <- orders.train[(is.na(orders.train$salutation)),] #351 obs
sum(missing10$returnShipment) # 112 returns 
mean(missing10$returnShipment) #0.3190883
binom.test(112, 351, p = .4824, alternative = c("two.sided"), conf.level = 0.95) # p-value = 6.023e-10
t.test(missing10$returnShipment, orders.train$returnShipment) # p-value = 2.016e-10

remove(missing3, missing11, missing5, missing10, colorMap)

#-----------------------------------------------#


#Recode & impute prior to making further transformations on our variables

# Recode ? to NA for color
orders.train$color <- mapvalues(orders.train$color, from = colorMap$original.color, to = colorMap$mapped)
orders.train$color[orders.train$color =="?"] <- NA

# Recode "not reported" to NA for salutation
orders.train$salutation[orders.train$salutation =="not reported"] <- NA

# Rows with missing delivery dates we are assuming were not delivered (they all show as 0 returns)
# Since you can't return what was not even delivered to you yet...
# We need to take these out - because these could affect parameter estimates
# At the same time, we apply a 0% return rate to these for the interim
orders.train <- orders.train[!(is.na(orders.train$deliveryDate)),]

#Mice is unable to impute on entire dataset (too many observations, becomes singular)
#So taking a subset of 100,000 observations (including those with missing observations)
#to perform imputation on

#lets copy this data over so we don't mess with the original
#Size has 122 levels, imputing using it would be meaningless
orders.missing <- orders.train[!complete.cases(orders.train),c(-2,-3,-5,-13) ]
orders.complete <- orders.train[complete.cases(orders.train),c(-2,-3,-5,-13)]

set.seed(2000)
#Need 54567 more observations to impute on for train file
orders.impute <- rbind(orders.missing, orders.complete[sample(nrow(orders.complete),size = 54567),])

#For the class file, just impute on everything
#Uncomment next line when performing imputation on final test data
#orders.impute <- rbind( orders.missing, orders.complete )

#MICE requires date fields be converted to numeric 
#Most date fields will not be used, so they are commented out but included here in case needed
#orders.impute$orderDate <- as.numeric(orders.impute$orderDate)
#orders.impute$deliveryDate <- as.numeric(orders.impute$deliveryDate)
orders.impute$dateOfBirth <- as.numeric(orders.impute$dateOfBirth)
#orders.impute$creationDate <- as.numeric(orders.impute$creationDate)

#Perform imputation
imputedTrain <- mice(orders.impute, m=1) 
imputedData <- complete(imputedTrain)

#Convert DOBs back to dates
imputedData$dateOfBirth <- as.Date(imputedData$dateOfBirth, origin = "1970-01-01")

#save imputed data back to our orders.train for all the observations missing values
orders.train <- merge( orders.train, imputedData, by.x = "orderItemID", 
                           by.y = "orderItemID", all.x = TRUE, suffixes = c("",".y"))

orders.train[is.na(orders.train$color), "color"] <-  orders.train[is.na(orders.train$color), "color.y"]
orders.train[is.na(orders.train$salutation), "salutation"] <-  orders.train[is.na(orders.train$salutation), "salutation.y"]
orders.train[is.na(orders.train$dateOfBirth), "dateOfBirth"] <-  orders.train[is.na(orders.train$dateOfBirth), "dateOfBirth.y"]
summary(orders.train$dateOfBirth)

#Get rid of the extra merged columns
orders.train <- orders.train[,1:16]

orders.class.Imputed <- orders.train

#write imputed date to file
# WARNING --- This will over-write the currently saved imputed data! 
#Uncomment once of the next two lines as appropriate depending on if peroforming this for training or test data
#save(orders.train, file = "ImputedOrders.RData")
#save(orders.class.Imputed, file = "orders_class_Imputed.rdata")
#This will load the imputed version of orders.train
#WARNING! This will overwrite your environments current version of orders.train
#test <- load("ImputedOrders.RData")

#------------------ END IMPUTATION-----------------#
orders.train <- orders.class.Imputed

#---------------------------#
#    More Transformations   #
#---------------------------#

# Add date diff variables
# Time from when order was placed to delivery, in days
orders.train$timeToDeliver <- as.numeric(difftime(orders.train$deliveryDate,orders.train$orderDate,unit="days"))
# Age of the account, in years but rounded to nearest tenth- seemed like a continuous variable was overkill here
orders.train$accountAge <- round(as.numeric(difftime(orders.train$orderDate,orders.train$creationDate,unit="weeks"))/52.25,1)
# want customer's age as an integer, similar to when you ask how old someone is
orders.train$customerAge <- floor(as.numeric(difftime(orders.train$orderDate,orders.train$dateOfBirth,unit="weeks"))/52.25)
# Check
# summary(orders.train[15:17])
# timeToDeliver should never be negative, and age should never be negative
orders.train$timeToDeliver <- ifelse(orders.train$timeToDeliver<0,NA,orders.train$timeToDeliver)
orders.train$customerAge <- ifelse(orders.train$customerAge<0,NA,orders.train$customerAge)
# age should also probably not be > 100 - what should we use for the cut-off?
orders.train$customerAge <- ifelse(orders.train$customerAge>100,NA,orders.train$customerAge)
# Recheck
# summary(orders.train[15:17])


#----------------------------------#
# Densities for all numeric (non-ID)
ggplot(orders.train,aes(x=price)) + geom_density(fill="grey") + ggtitle("Price Distribution")
ggplot(orders.train,aes(x=timeToDeliver)) + geom_density(fill="grey") + ggtitle("Delivery Time Distribution")
ggplot(orders.train,aes(x=customerAge)) + geom_density(fill="grey") + ggtitle("Age Distribution")
ggplot(orders.train,aes(x=accountAge)) + geom_density(fill="grey") + ggtitle("Account Age Distribution")
#----------------------------------#





#----------------------------------------#
#   Transformation code applied to Size  #
#----------------------------------------#

#Identify 'high risk' and 'low risk' sizes
orders.train$sizeHighRisk <- orders.train$size == '40' | orders.train$size == '41'  | orders.train$size == '42'
orders.train$sizeLowRisk <- orders.train$size == 'unsized' 


# Sizing recodes - creating a table with frequencies to work from and going to remove sizes as I recode them
# There may be some errors here- for example, Euro children's sizes start at 50, but some conversions go up to size 52 for men's suits, etc
##### not sure how many items this affects, but we could check the range of values for those items to see which class they belong to?
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

#  Maybe sizes < 20 are US type sizes - either for shoes or women's dresses 
#  both are difficult to size without trying on
orders.train$sizeShoeDress <- ifelse(as.numeric(as.character(orders.train$size))>=1 & as.numeric(as.character(orders.train$size))<=20,as.numeric(as.character(orders.train$size)),NA)
size.table <- size.table[-which(as.numeric(as.character(size.table$size))>=1 & as.numeric(as.character(size.table$size))<=20),]

#-------------------------------#
# CAN'T GET CODE TO WORK HERE#
# Plus sizes different in Shoe / Dress size range?
#orders.train$sizePlus <- ifelse(as.character(orders.train$size)=='2+' & as.character(orders.train$size)=='3+' & as.character(orders.train$size)=='4+' & as.character(orders.train$size)=='5+' 
#                                & as.character(orders.train$size)=='6+' & as.character(orders.train$size)=='7+' & as.character(orders.train$size)=='8+' & as.character(orders.train$size)=='9+' 
#                                & as.character(orders.train$size)=='10+' & as.character(orders.train$size)=='11+' & as.character(orders.train$size)=='12+' & as.character(orders.train$size)=='13+',
#                                as.character(orders.train$size), NA)
#size.table <- size.table[-which(as.character(orders.train$size)=='2+' & as.character(orders.train$size)=='3+' & as.character(orders.train$size)=='4+' & as.character(orders.train$size)=='5+' 
#                                & as.character(orders.train$size)=='6+' & as.character(orders.train$size)=='7+' & as.character(orders.train$size)=='8+' & as.character(orders.train$size)=='9+' 
#                                & as.character(orders.train$size)=='10+' & as.character(orders.train$size)=='11+' & as.character(orders.train$size)=='12+' & as.character(orders.train$size)=='13+'),]


#orders.train$sizePlus <- ifelse(as.numeric(as.character(orders.train$size))=='2+' & as.numeric(as.character(orders.train$size))=='3+' & as.numeric(as.character(orders.train$size))=='4+' & as.numeric(as.character(orders.train$size))=='5+' 
#                                & as.numeric(as.character(orders.train$size))=='6+' & as.numeric(as.character(orders.train$size))=='7+' & as.numeric(as.character(orders.train$size))=='8+' & as.numeric(as.character(orders.train$size))=='9+' 
#                                & as.numeric(as.character(orders.train$size))=='10+' & as.numeric(as.character(orders.train$size))=='11+' & as.numeric(as.character(orders.train$size))=='12+' & as.numeric(as.character(orders.train$size))=='13+',
#                                as.numeric(as.character(orders.train$size)), NA)
#size.table <- size.table[-which(as.character(orders.train$size)=='2+' & as.character(orders.train$size)=='3+' & as.character(orders.train$size)=='4+' & as.character(orders.train$size)=='5+' 
#                                & as.character(orders.train$size)=='6+' & as.character(orders.train$size)=='7+' & as.character(orders.train$size)=='8+' & as.character(orders.train$size)=='9+' 
#                                & as.character(orders.train$size)=='10+' & as.character(orders.train$size)=='11+' & as.character(orders.train$size)=='12+' & as.character(orders.train$size)=='13+'),]
#-------------------------------#

# Remaining
orders.train$sizeOther <- ifelse(is.na(orders.train$sizeLetter) & is.na(orders.train$sizePant) & is.na(orders.train$sizeChild) & is.na(orders.train$sizeShoeDress), as.character(orders.train$size),NA)

# check
table(orders.train$sizeLetter)
table(orders.train$sizePant)
table(orders.train$sizeChild)
table(orders.train$sizeShoeDress)
table(orders.train$sizeOther)
#table(orders.train$sizePlus)

# Identification Markers for various clothing type
orders.train$LetterSize <- ifelse(is.na(orders.train$sizeLetter),0,1)
orders.train$Pants <- ifelse(is.na(orders.train$sizePant),0,1)
orders.train$ChildSize <- ifelse(is.na(orders.train$sizeChild),0,1)
orders.train$ShoeDress <- ifelse(is.na(orders.train$sizeShoeDress),0,1)
#orders.train$PlusSize <- ifelse(is.na(orders.train$sizePlus),0,1)


##### Euro Women's Dress sizes seem to range between 28 and 54 - maybe we leave these alone and in 'other'
#  https://www.google.com/search?q=dress+sizes&tbm=isch&tbo=u&source=univ&sa=X&ei=giRfU_LmMKa6yQGu4IHwDw&ved=0CCgQsAQ&biw=1080&bih=484#q=european+dress+sizes&tbm=isch&facrc=_&imgrc=VvapZ5APyTOnrM%253A%3B-WKQ1Hty0kzBiM%3Bhttp%253A%252F%252Fwww.europeword.com%252Fblog%252Fwp-content%252Fuploads%252Feuropean-dress-sizes.jpg%3Bhttp%253A%252F%252Fwww.europeword.com%252Fblog%252Feurope%252Feuropean-dress-sizes%252F%3B756%3B479
#  Tried to look up men's shirt sizes too, but there is a lot of overlap in the systems here
#####
#### DO WE NEED TO ADD PLUS SIZES HERE??? ######

#--------------------------------#
# Size Modes                     #
# need to code application still #
#--------------------------------#

# Add mode function - note that this only gives one mode if there is more than one
mymode <- function(x){
  names(sort(-table(as.character(x))))[1]
}
custMode1 <- summaryBy(toupper(as.character(orders.train$size)) ~ orders.train$customerID, orders.train, FUN=mymode)
custMode2 <- summaryBy(sizeLetter ~ customerID, orders.train[-which(is.na(orders.train$sizeLetter)),], FUN=mymode)
custMode3 <- summaryBy(sizePant ~ customerID, orders.train[-which(is.na(orders.train$sizePant)),], FUN=mymode)
custMode4 <- summaryBy(sizeChild ~ customerID, orders.train[-which(is.na(orders.train$sizeChild)),], FUN=mymode)
custMode5 <- summaryBy(sizeOther ~ customerID, orders.train[-which(is.na(orders.train$sizeOther)),], FUN=mymode)
custMode6 <- summaryBy(sizeShoeDress ~ customerID, orders.train[-which(is.na(orders.train$sizeShoeDress)),], FUN=mymode)
custMode <- merge(custMode1,custMode2,by="customerID",all=T)
custMode <- merge(custMode,custMode3,by="customerID",all=T)
custMode <- merge(custMode,custMode4,by="customerID",all=T)
custMode <- merge(custMode,custMode5,by="customerID",all=T)
custMode <- merge(custMode,custMode6,by="customerID",all=T)
names(custMode) <- c("customerID","sizeMode","szLetterMode","szPantMode", "szChildMode", "szOtherMode", "szShoeDressMode")
# Merge back into original file, then drop the unnecessary data frames to clean up the workspace
##### Should we be looking at mode for each clothing type, and just noting if the next order does not belong to that mode for that type,
##### or is this just getting too complicated?
orders.train <- merge(orders.train,custMode,by="customerID",all=T)
remove(custMode,custMode1,custMode2,custMode3,custMode4,custMode5,size.table)

####### END SIZING CODE #########
#---------------------------------------------#

# Add holiday/bday flags
# NOTE: ALL OBS ARE MARKED '1' FOR BDAY
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
orders.train <- merge(orders.train,numItems,by=c("customerID","orderDate"),all.x=TRUE)
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

orders.class.Imputed.mid <- orders.train
orders.class.Imputed.temp <- orders.class.Imputed.mid
load("imputedOrdersPostTransformation.rdata")
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
orders.class.Imputed <- merge(orders.class.Imputed.temp,riskyManuf,
                                   all.x = TRUE, by="manufacturerID")
summary(orders.class.Imputed)
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
orders.class.Imputed.temp <- merge(orders.class.Imputed.temp,riskyItems,
                                   all.x = TRUE, by="itemID")
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
orders.class.Imputed.temp <- merge(orders.class.Imputed.temp,riskyCust,
                                   all.x = TRUE, by="customerID")


remove(riskyManuf,riskyItems,riskyCust)

# Check if items are always the same price (expect they're not, but wanted to verify before coding more)
# Using a merge because if I try to do quantile and mean in 1 step, the labels aren't clear
itemPricing <- merge(summaryBy(price ~ itemID,orders.train,FUN=quantile),
    summaryBy(price ~ itemID,orders.train,FUN=mean),by="itemID")
View(itemPricing) # confirmed, going to attach to the orders.train data frame so we can later flag 
orders.train <- merge(orders.train, itemPricing, by="itemID")
orders.train$difFromMeanPrice = orders.train$price - orders.train$price.mean
remove(itemPricing)

itemPricing <- merge(summaryBy(price ~ itemID,orders.class.Imputed.temp,FUN=quantile),
                     summaryBy(price ~ itemID,orders.class.Imputed.temp,FUN=mean),by="itemID")
orders.class.Imputed.temp <- merge(orders.class.Imputed.temp, itemPricing, by="itemID" )
orders.class.Imputed.temp$difFromMeanPrice = orders.class.Imputed.temp$price - orders.class.Imputed.temp$price.mean

# Look at mean of returnShipment for each price point
# Currently saving this out as a separate table because I'm not entirely sure what to do with it
returnsByPrice <- summaryBy(returnShipment ~ itemID + price, orders.train, FUN=c(length,mean))

# Create OrderID variable
orders.table <- summaryBy(returnShipment ~ orderDate + customerID, orders.train, FUN=mean)
orders.table$orderID <- 1:nrow(orders.table)
orders.table <- orders.table[,-3]
orders.train <- merge(orders.train,orders.table,by=c("customerID","orderDate"))
remove(orders.table)

orders.class.Imputed <- orders.class.Imputed.temp
str(orders.train)

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

#Save command for after applying all the transformations

#The "most transforms" represents that I'm not sure how to 
#get the dupItems variable implemented along with the orderID, everything else
#should be in there.

#Finally, before saving --> update any NA values in the imputed data with the mean value
#from the training dataset

summary(orders.train)
summary(orders.class.Imputed)

#For many of the new items/manufacturers, do not have enough good data to impute
# so we will fill in these small number of NAs with mean values for model
orders.class.Imputed[is.na(orders.class.Imputed$timeToDeliver),"timeToDeliver"] <- 10.72
orders.class.Imputed[is.na(orders.class.Imputed$numCustOrders), "numCustOrders" ] <- 20.89
orders.class.Imputed[is.na(orders.class.Imputed$numCustReturns), "numCustReturns"] <- 0.5254
#Assuming customers are not risky unless they have demonstrated to be so
orders.class.Imputed[is.na(orders.class.Imputed$custRiskFlag), "custRiskFlag"] <- 0
orders.class.Imputed[is.na(orders.class.Imputed$numItemReturns),"numItemReturns"] <- 0.5254
orders.class.Imputed[is.na(orders.class.Imputed$numItemOrders), "numItemOrders"] <- 632.4

summary(orders.class.Imputed)

save(orders.class.Imputed, file = "orders_class_Imputed_FINAL_noNAs.rdata")

