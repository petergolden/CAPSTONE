# Scale Function


# Load data

load("imputedOrdersPostTransformation.rdata")
summary(orders.train)

#eliminate cases where timeToDeliver is NA
orders.train <- orders.train[complete.cases(orders.train[,17]),]
summary(orders.train)
# For final prediction vector, Will need to manually add prediction of returnShipment=0 for incomplete shipments
# FOR WORKING WITH orders.class.Imputed

# This will convert variables into z-scores

head(orders.train)

#color <- scale(as.numeric(orders.train$color), center= TRUE, scale = TRUE)
#salutation <- scale(as.numeric(orders.train$salutation), center= TRUE, scale = TRUE)


timeToDeliver <- scale(orders.train$timeToDeliver, center= TRUE, scale = TRUE)
accountAge <- scale(orders.train$accountAge, center= TRUE, scale = TRUE)
holidayFlag <- scale(orders.train$holidayFlag, center= TRUE, scale = TRUE)
LetterSize <- scale(orders.train$LetterSize, center= TRUE, scale = TRUE)
ChildSize <- scale(orders.train$ChildSize, center= TRUE, scale = TRUE)
ShoeDress <- scale(orders.train$ShoeDress, center= TRUE, scale = TRUE)
sizeHighRisk <- scale(orders.train$sizeHighRisk, center= TRUE, scale = TRUE)
sizeLowRisk <- scale(orders.train$sizeLowRisk, center= TRUE, scale = TRUE)
difFromMeanPrice <- scale(orders.train$difFromMeanPrice, center= TRUE, scale = TRUE)
price <- scale(orders.train$price, center= TRUE, scale = TRUE)
numItemsInOrder <- scale(orders.train$numItemsInOrder, center= TRUE, scale = TRUE)
numCustOrders <- scale(orders.train$numCustOrders, center= TRUE, scale = TRUE)
numCustReturns <- scale(orders.train$numCustReturns, center= TRUE, scale = TRUE)
custRiskFlag <- scale(orders.train$custRiskFlag, center= TRUE, scale = TRUE)
numItemReturns <- scale(orders.train$numItemReturns, center= TRUE, scale = TRUE)
numItemOrders <- scale(orders.train$numItemOrders, center= TRUE, scale = TRUE)
numManufOrders <- scale(orders.train$numManufOrders, center= TRUE, scale = TRUE)


# Data Check

summary(orders.train$salutation) 

company <- ifelse(orders.train$salutation=='Company',1,0)
family <- ifelse(orders.train$salutation=='Family',1,0)
Mr <- ifelse(orders.train$salutation=='Mr',1,0)

summary(company)
summary(family)
summary(Mr)


summary(orders.train$color) 
brown <- ifelse(orders.train$color=='brown',1,0)
purple <- ifelse(orders.train$color=='purple',1,0)
weird <- ifelse(orders.train$color=='weird',1,0)
grey <- ifelse(orders.train$color=='grey',1,0)
pink <- ifelse(orders.train$color=='pink',1,0)
green <- ifelse(orders.train$color=='green',1,0)
blue <- ifelse(orders.train$color=='blue',1,0)
black <- ifelse(orders.train$color=='black',1,0)
red <- ifelse(orders.train$color=='red',1,0)
white <- ifelse(orders.train$color=='white',1,0)
yellow <- ifelse(orders.train$color=='yellow',1,0)
orange <- ifelse(orders.train$color=='orange',1,0)

summary(brown) 
summary(purple) 
summary(weird) 
summary(grey) 
summary(pink) 
summary(green) 
summary(blue) 
summary(black) 
summary(red) 
summary(white) 
summary(yellow) 
summary(orange)


summary(timeToDeliver) 
summary(accountAge) 
summary(holidayFlag) 
summary(LetterSize) 
summary(ChildSize) 
summary(ShoeDress) 
summary(sizeHighRisk) 
summary(sizeLowRisk) 
summary(difFromMeanPrice) 
summary(price) 
summary(numItemsInOrder) 
summary(numCustOrders) 
summary(numCustReturns) 
summary(custRiskFlag) 
summary(numItemReturns) 
summary(numItemOrders) 
summary(numManufOrders) 


###################################################
#-------------------------------------------------#
#                  FINAL MODEL                    #
#-------------------------------------------------#
#   Model Produced by Backwards Elimination       #
#-------------------------------------------------#
###################################################

BE.LR.Model <- glm(formula = returnShipment ~ timeToDeliver + 
                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                     numItemOrders + numManufOrders, family = binomial(link = logit), 
                   data = orders.train)

summary(BE.LR.Model)


LR.Model.Scaled <- glm(formula = returnShipment ~ timeToDeliver + 
                    brown + purple + weird + grey + pink + green + blue + black + red + white + yellow + orange +
                     company + family + Mr +
                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                     numItemOrders + numManufOrders, family = binomial(link = logit), 
                   data = orders.train)

summary(LR.Model.Scaled)

rm(orders.train, BE.LR.Model, LR.Model.Scaled, Mr, company, family, Mr,
   brown, purple, weird, grey, pink, green, blue, black, red, white, yellow, orange)

gc()
memory.size()
memory.limit()

