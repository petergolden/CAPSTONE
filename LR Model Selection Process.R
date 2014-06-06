#-----------------------------------------------#
# File for Logistic Regression MODELING PROCESS #
#-----------------------------------------------#

# Add Libraries
library(ROCR)
library(caret)


load("imputedOrdersPostTransformation.rdata")
summary(orders.train)

#eliminate cases where timeToDeliver is NA
orders.train <- orders.train[complete.cases(orders.train[,17]),]
# Oops, forgot to impute custAge NAs - let's see if it's even necessary to backtrack... 
orders.train <- orders.train[complete.cases(orders.train[,19]),]

summary(orders.train)

# We created a lot of variables, many of which were used as intermediate steps to creating other variables.  
# The full model excludes those variables created for intermediate steps
# Given Variables excluded from the full model were:
  # customerID, orderDate, itemID, manufacturerID, orderItemID, deliveryDate, size (just too large unchanged)
  # dateOfBirth, creationDate, and orderID
# Created variables that we excluded were:
  # all size type markers, which we converted into binary variables
  # All size mode markers, which were a start to an idea we didn't have time to pursue
  # All interquartile and mean price values, which we simply used to calculate diffFromMean

# For final prediction vector, Will need to manually add prediction of returnShipment=0 for incomplete shipments
# FOR WORKING WITH orders.class.Imputed


#------------------------#
#  Train & Test Split    #
#------------------------#

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

#------END TRAIN/TEST SPLIT-------#
# eliminate cases where timeToDeliver is NA 
# To test against produced LR model, need to do after train / test split, or else seed will select different observations
# Test shows zero difference in model coefficients - yay!
# Only (minor) differences in Final Model from initial run will be due to seed having a different placement in train/test regimen
# Therefore seed number is unimportant overall
# train <- train[complete.cases(train[,17]),]
# test <- test[complete.cases(test[,17]),] 
# summary(train)
# summary(test)

remove(orders.train) # To clean workspace 


#-------------------------------------------#
#             Logistic Regression           #
#-------------------------------------------#
# Look at Week 3 assignment of Predict 412

str(train)

#------ LR "JB Trial Model"----------#
returns.lr <- glm(returnShipment ~ color + timeToDeliver 
                  + salutation + state
                  + accountAge + customerAge 
                  + holidayFlag + bdayFlag 
                  + LetterSize + Pants + ChildSize + ShoeDress 
                  + difFromMeanPrice + price  
                  + numCustOrders + numCustReturns + custRiskFlag 
                  + numItemReturns + numItemOrders + itemRiskFlag
                  + numManufOrders + numManufReturns + manufRiskFlag,
                  family=binomial(link=logit), data=train)
summary(returns.lr)

# TO Get ROC Curves
# get predictions from model 
# NEED TO KNOW WHAT WE CALL TRAIN AND TEST DATA SETS
predict.train.logistic <- predict(returns.lr, train, type="response")
predict.test.logistic <- predict(returns.lr, test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the model ROC curves
pdf(file = "LR_model_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

plot(train.logistic.roc, col = "darkgreen", main = "ROC Curves for Logistic Regression Model")
plot(test.logistic.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.logistic.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.logistic.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))
dev.off()

str(predict.test.logistic) 


# ----------------LR "Full Model" ----------------------------#
# Had to add variables manually as I was getting an error just using returnShipment~.
# Also realized some variables, particularly date vaiables and ID number variables (in number format)
# did not really behave like continuous variables
# or variables where value on a continuum was meaningful
# Didn't want spurious results so eliminated those first
# careful here - full model sucks resources
# I think there are multicollinearity issues here
# Let's see how BE goes, but we might need to do some manual manipulation 
# CustomerAge has some NA's - need to check impute for this variable
returns.full.lr <- glm(returnShipment ~ color + timeToDeliver 
                       + salutation + state
                       + accountAge + customerAge
                       + holidayFlag + bdayFlag 
                       + LetterSize + Pants + ChildSize + ShoeDress + sizeHighRisk + sizeLowRisk
                       + difFromMeanPrice + price  
                       + numItemsInOrder
                       + numCustOrders + numCustReturns + custRiskFlag 
                       + numItemReturns + numItemOrders + itemRiskFlag
                       + numManufOrders + numManufReturns + manufRiskFlag,
                       family=binomial(link=logit), data=train)
summary(returns.full.lr)

# generate ROC on FULL MODEL
predict.train.logistic <- predict(returns.full.lr, train, type="response")
predict.test.logistic <- predict(returns.full.lr, test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the full model ROC curves
pdf(file = "LR_model_Full_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

plot(train.logistic.roc, col = "darkgreen", main = "ROC Curves for Logistic Regression Model")
plot(test.logistic.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.logistic.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.logistic.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))
dev.off()


#---------- Backwards Elimination Selection Procedure -----------#
# use default AIC measure
# Note step function uses full model defined above 
returns.backward.lr <- step(returns.full.lr)
summary(returns.backward.lr)


# TO Get ROC Curves
# get predictions from model 
predict.train.logistic <- predict(returns.backward.lr, train, type="response")
predict.test.logistic <- predict(returns.backward.lr, test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the model ROC curves
pdf(file = "LR_model_backward_elim_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

plot(train.logistic.roc, col = "darkgreen", main = "ROC Curves for Logistic Regression Model")
plot(test.logistic.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.logistic.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.logistic.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))
dev.off()

str(predict.test.logistic) 

# Eliminated variables were: 
# state + bdayFlag + Pants + itemRiskFlag + numManufReturns + manufRiskFlag,
# Also note, many variables we created were specifcally designed to be included in the calculations
# of other variables, and would have no benefit on a standalone basis.  
# These include variables such as custPriceMin or custPriceMax.

###################################################
#-------------------------------------------------#
#                  FINAL MODEL                    #
#-------------------------------------------------#
#   Model Produced by Backwards Elimination       #
#-------------------------------------------------#
###################################################

BE.LR.Model <- glm(formula = returnShipment ~ color + timeToDeliver + salutation + 
                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                     numItemOrders + numManufOrders, family = binomial(link = logit), 
                   data = train)

# WITH customerAge replacing sizeHighRisk - VERY LITTLE DIFFERENCE IN RESULTS
BE.LR.Model <- glm(formula = returnShipment ~ color + timeToDeliver + salutation + customerAge + 
                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                     sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                     numItemOrders + numManufOrders, family = binomial(link = logit), 
                   data = train)


summary(BE.LR.Model)

# TO Get ROC Curves
# get predictions from model 
predict.train.logistic <- predict(BE.LR.Model, train, type="response")
predict.test.logistic <- predict(BE.LR.Model, test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the model ROC curves
pdf(file = "BE_LR_Model_ROC2.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

plot(train.logistic.roc, col = "darkgreen", main = "ROC Curves for Logistic Regression Model")
plot(test.logistic.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.logistic.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.logistic.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))
dev.off()

str(predict.test.logistic) 

# save workspace
save.image(file="Logistic_workspace.RData")


#-----------------------------------#
#  Prediction Vector for Class Set  #
#-----------------------------------#
#Pete's Comments:
#For the variables that are fed into the model and have some NAs, 
#I replaced the NAs with the mean value from the orders.train dataset. 
#The exception to this is the riskycustomer flag, which I set to 0 
#(assume a customer is not risky unless they prove to be so).

load("orders_class_Imputed_FINAL_noNAs.rdata")
summary(orders.class.Imputed)

# Identify and Separate undelivereds
undelivereds <- subset(orders.class.Imputed, is.na(deliveryDate))
summary(undelivereds)

# create prediction vector of 0s for return prob for undelivereds, merge
returnShipment<-rep(c(0),4268)
undelivereds<-cbind(undelivereds, returnShipment)
P1<-cbind(undelivereds$orderItemID,undelivereds$returnShipment)
summary(P1)
length(P1)/2

# Eliminate "undelivereds" from the class set so we only predict complete cases for "delivereds"
orders.class.Imputed <- orders.class.Imputed[complete.cases(orders.class.Imputed[,6]),]
summary(orders.class.Imputed)

# create prediction vector for delivereds
predict.class.logistic <- predict(BE.LR.Model, orders.class.Imputed, type="response")
summary(predict.class.logistic)
str(predict.class.logistic)
P2<- cbind(orders.class.Imputed$orderItemID,predict.class.logistic)
str(P2)
length(P2)/2

# AT LAST!!!!  The Final Predictions!!!!
Final.Predictions<-rbind(P1,P2)
summary(Final.Predictions)
length(Final.Predictions)/2

write.table(Final.Predictions, file="c:/users/Jim Braun/My Documents\\finalpredictions.txt", row.names=FALSE,sep=";")

#write.csv2(result, file ="F:\\filename.csv",row.names=FALSE)
#write.csv2 use sep=";" and dec="," as default.
#write.table(result, file ="F:\\filename.csv",row.names=FALSE,sep=";")


# Alternate approach
# NEED TO CREATE IF/ELSE CODE ON DELIVERY DATE TO CALL ALL UNDELIVEREDS '0'
# AND THEN CBIND THOSE TO THE PREDICTION VECTOR

# rmse(orders.test$returnShipment,predict(logistic.model,orders.test))

#------------------#
# Confusion Matrix #  
#------------------#
# We need to convert preds to actual choice; introduce 'cut'
# After several tested iterations, selected a p=.5 cutoff after review of ROC
predictions<-cut(predict.test.logistic, c(-Inf,0.5,Inf), labels=c("Keep","Return"))
# Now have a look - classes are assigned
str(predictions)
summary(predictions)
# compare to test$pick to ensure same # of levels and obs
# Need to impute or eliminate observations with NAs or else have above issue
str(test$returnShipment)
summary(test$returnShipment)
LRactuals <- factor(test$returnShipment,
                    levels = c(0,1),
                    labels = c("Keep", "Return"))
#Needs caret library
confusionMatrix(predictions, LRactuals)


#-----------------------#
# And then a lift chart #
#-----------------------#
pdf("LR_Lift_Chart.pdf")
train.lift.LR <- performance(train.logistic.pred, "lift","rpp")
#train.liftforest <- performance(train.rocforest.prediction, "lift","rpp")
test.lift.LR <- performance(test.logistic.pred, "lift","rpp")
plot(train.lift.LR, col="green", main = "Lift Curve Logistic Regression")
#plot(train.liftforest, col="blue", main = "Lift Curve Random Forest")
plot(test.lift.LR, col="red", add = TRUE)
legend("bottomleft",c("Sample","Test"),fill=(c("green","red")))
#legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()


#clean workspace for next algorithm (KT- leave test.logistic.roc/test.logistic.pred for final comparison)
remove(predict.test.logistic, predict.train.logistic, predictions, returns.lr, 
       test.logistic.auc, #test.logistic.pred, test.logistic.roc,
       train.logistic.roc, train.logistic.pred, train.logistic.auc,
       test.legend,train.legend, LRactuals, train.lift.LR, test.lift.LR, 
       BE.LR.Model)
# to remove later if helpful: test.logistic.pred, test.logistic.pred, test.logistic.roc


gc()
memory.size()
memory.limit()

