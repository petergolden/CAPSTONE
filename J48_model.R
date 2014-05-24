# J48

# Add Libraries
library(ROCR)
library(caret)
library(party) # for KT's Random Forest syntax


load("imputedOrdersPostTransformation.rdata")
summary(orders.train)

#eliminate cases where timeToDeliver is NA
orders.train <- orders.train[complete.cases(orders.train[,17]),]
summary(orders.train)
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


#---------------------------------#
#         Decision Trees          #
#---------------------------------#
# J48 (based on Quinlan's C4.5)
# to run j48 in RWeka
library(RWeka)
# To use predictions f(x)
library(ROCR)
# Careful this takes a few minutes!
remove(orders.train) # clean space

#---SAME VARIABLES AS BE LR----#
# J48 cannot handle numeric class - have to convert to factor
RS <- as.factor(train$returnShipment)
j48_selected <- J48(RS ~ color + timeToDeliver + salutation + 
                      accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                      sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                      numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                      numItemOrders + numManufOrders,
                    data=train)
j48_selected
summary(j48_selected)


#------------------#
#       RMSE       #
#------------------#
predict.train.J48 <- predict(j48_selected, train, type="probability")
predict.test.J48 <- predict(j48_selected, test, type="probability")

rmse <- function(observed,predicted) {
  sqrt(mean((observed-predicted)^2))
}

rmse(test$returnShipment,predict.test.J48) #0.615  - Also Ouch!

# save workspace
save.image(file="J48_workspace.RData")

# NOTE: CANNOT DO a ROC Curve using ROCR if prediction is not a continuous variable #
# So it doesn't work with J48, which produces a discrete binary classification #

#-----------------------#
# And then a lift chart #
#-----------------------#
train.J48.pred <- prediction(predict.train.J48[,2], train$returnShipment)
test.J48.pred <- prediction(predict.test.J48[,2], test$returnShipment)

#okay to here; not after

train.lift.J48 <- performance(train.J48.pred, "lift","rpp")
test.lift.J48 <- performance(test.J48.pred, "lift","rpp")

pdf("J48_Lift_Chart.pdf")
plot(train.lift.J48, col="green", main = "Lift Curve J48")
plot(test.lift.J48, col="red", add = TRUE)
legend("bottomleft",c("Sample","Test"),fill=(c("green","red")))
#legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()





remove(predict.train.J48, predictions, 
       test.logistic.auc, train.J48.pred, train.J48.roc,train.J48.auc,
       test.J48.pred, test.J48.roc, test.J48.auc,
       train.legend, test.legend, j48_selected, RS)


# save workspace
save.image(file="J48_workspace2.RData")


#---------------------#
#     ROC CURVES      #
#---------------------#

predict.train.J48 <- predict(j48_selected, train, type="response")
predict.test.J48 <- predict(j48_selected, test, type="response")

train.J48.pred <- prediction(predict.train.J48[,2], train$returnShipment)
train.J48.roc <- performance(train.J48.pred, "tpr","fpr")
train.J48.auc <- (performance(train.J48.pred, "auc"))@y.values

test.J48.pred <- prediction(predict.test.J48[,2], test$returnShipment)
test.J48.roc <- performance(test.J48.pred, "tpr","fpr")
test.J48.auc <- (performance(test.J48.pred, "auc"))@y.values

# plot the selected model ROC curves
pdf(file = "J48_model_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

plot(train.J48.roc, col = "darkgreen", main = "ROC Curves for J48 Tree")
plot(test.J48.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.J48.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.J48.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))
dev.off()

save.image(file="J48_workspace3.RData")


remove(predict.train.J48, train, 
       train.J48.auc, train.J48.pred, train.J48.roc, train.lift.J48,
       test.J48.roc, test.J48.auc, test.lift.J48,
       train.legend, test.legend, RS)

# keep
# test
# j48_selected
# predict.test.J48
# test.J48.pred


save.image(file="J48_rmse_predicts.RData")


#------------------------ END J48--------------------------------#