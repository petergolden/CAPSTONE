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

#summary(color) 
#summary(salutation) 

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



#------------------------#
#  Train & Test Split    #
#------------------------#
smp_size <- floor(0.7 * max(orders.train$orderID))
set.seed(498)
train_ind <- sample(seq_len(max(orders.train$orderID)), size = smp_size)
orders.train$trainTest <- train_ind[orders.train$orderID]
train <- orders.train[which(orders.train$trainTest>0), ]
test <- orders.train[-which(orders.train$trainTest>0), ]
remove(smp_size,train_ind)

remove(orders.train) # To clean workspace for 'hungry' algorithms


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
                   data = train)

summary(BE.LR.Model)


#BE.LR.Model <- glm(formula = returnShipment ~ timeToDeliver + color + salutation
#                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
#                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
#                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
#                     numItemOrders + numManufOrders, family = binomial(link = logit), 
#                   data = train)

#summary(BE.LR.Model)

