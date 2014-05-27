# Add Libraries
library(ROCR)
library(caret)
library(ipred)

load("imputedOrdersPostTransformation.rdata")

#eliminate cases where timeToDeliver/customerAge is NA
orders.train <- orders.train[complete.cases(orders.train[,17]),]
orders.train <- orders.train[complete.cases(orders.train[,'customerAge']),]
# creating an even smaller dataset by only having variables used
# this drops actual dates and a lot of the size stuff
str(orders.train)
orders <- cbind(orders.train[1],orders.train[3:5],orders.train[7:10],
                orders.train[12],orders.train[14:19],
                orders.train[25:28],orders.train[35:47],
                orders.train[54:55])
str(orders)

#train/test split, 70/30
set.seed(498)
train_ind <- sample(seq_len(max(orders$orderID)), size = floor(0.7 * max(orders$orderID)))
trainTest <- train_ind[orders$orderID]
train <- orders[which(trainTest>0), ]
test <- orders[-which(trainTest>0), ]
remove(trainTest,train_ind,orders.train,orders)
gc()

save.image(file="KT_initial_workspace.RData")

summary(train)

################ Bagging Model - Full Model ###################
# Reference:
# http://heuristically.wordpress.com/2009/12/23/compare-performance-machine-learning-classifiers-r/

# fit a bagging model to training set 
bagging.model <- bagging(returnShipment ~ ., data = train)
summary(bagging.model)

predict.bag.train <- predict(bagging.model, type="prob")
train.rocbag.prediction <- prediction(predict.bag.train, train$returnShipment)
predict.bag.test <- predict(bagging.model, type="prob", newdata=test)
test.rocbag.prediction <- prediction(predict.bag.test, test$returnShipment)

# true positive rate and false positive rate for ROC curve plotting
train.rocbag <- performance(train.rocbag.prediction, "tpr","fpr")
test.rocbag <- performance(test.rocbag.prediction, "tpr","fpr")

# do the ROC plot
pdf("Bagging_ROC_Full.pdf")
plot(train.rocbag, col="blue", main = "ROC Bagging")
plot(test.rocbag, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Training: AUC =",round(as.numeric(
  performance(train.rocbag.prediction,"auc")@y.values),4)),
  paste("Test: AUC =",round(as.numeric(
    performance(test.rocbag.prediction,"auc")@y.values),4))),
  fill=(c("blue","red")))
dev.off()

# And then a lift chart
pdf("Bagging_Lift_Full.pdf")
train.liftbag <- performance(train.rocbag.prediction, "lift","rpp")
test.liftbag <- performance(test.rocbag.prediction, "lift","rpp")
plot(train.liftbag, col="blue", main = "Lift Curve Bagging")
plot(test.liftbag, col="red", add = TRUE)
legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()

rmse <- function(observed,predicted) {
  sqrt(mean((observed-predicted)^2))
}

rmse(test$returnShipment,predict.bag.test) # 0.4079

# clean up everything except stuff for combined ROC/rmse:
# test.rocbag
# test.rocbag.prediction
# predict.bag.test
# test
remove(train,bagging.model,predict.bag.train,test.liftbag,
       train.rocbag,train.rocbag.prediction,train.liftbag)

save.image("bagging_full.RData")

#------------------#
# Confusion Matrix #  
#------------------#

load("bagging.RData")
# Using a p=.5 cutoff initially
predictions<-cut(predict.bag.test, c(-Inf,0.5,Inf), labels=c("Keep","Return"))
bagActuals <- factor(test$returnShipment,
                     levels = c(0,1),
                     labels = c("Keep", "Return"))
confusionMatrix(predictions, bagActuals)

# Get r squared
1 - sum((test$returnShipment-predict.bag.test)^2)/
  sum((test$returnShipment-mean(test$returnShipment))^2)

################ Bagging Model - LR BE Variables ###################
# Reference:
# http://heuristically.wordpress.com/2009/12/23/compare-performance-machine-learning-classifiers-r/

# fit a bagging model to training set 
# using variables from LR BE
bagging.model <- bagging(returnShipment ~ color + timeToDeliver 
                         + salutation + accountAge + holidayFlag 
                         + LetterSize + ChildSize + ShoeDress + sizeHighRisk + sizeLowRisk
                         + difFromMeanPrice + price  + numItemsInOrder
                         + numCustOrders + numCustReturns + custRiskFlag 
                         + numItemReturns + numItemOrders + numManufOrders
                         , data = train)
summary(bagging.model)

predict.bag.train <- predict(bagging.model, type="prob")
train.rocbag.prediction <- prediction(predict.bag.train, train$returnShipment)
predict.bag.test <- predict(bagging.model, type="prob", newdata=test)
test.rocbag.prediction <- prediction(predict.bag.test, test$returnShipment)

# true positive rate and false positive rate for ROC curve plotting
train.rocbag <- performance(train.rocbag.prediction, "tpr","fpr")
test.rocbag <- performance(test.rocbag.prediction, "tpr","fpr")

# do the ROC plot
pdf("Bagging_ROC.pdf")
plot(train.rocbag, col="blue", main = "ROC Bagging")
plot(test.rocbag, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Training: AUC =",round(as.numeric(
  performance(train.rocbag.prediction,"auc")@y.values),4)),
  paste("Test: AUC =",round(as.numeric(
    performance(test.rocbag.prediction,"auc")@y.values),4))),
  fill=(c("blue","red")))
dev.off()

# And then a lift chart
pdf("Bagging_Lift.pdf")
train.liftbag <- performance(train.rocbag.prediction, "lift","rpp")
test.liftbag <- performance(test.rocbag.prediction, "lift","rpp")
plot(train.liftbag, col="blue", main = "Lift Curve Bagging")
plot(test.liftbag, col="red", add = TRUE)
legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()

rmse <- function(observed,predicted) {
  sqrt(mean((observed-predicted)^2))
}

rmse(test$returnShipment,predict.bag.test) # 0.4082

# clean up everything except stuff for combined ROC/rmse:
# test.rocbag
# test.rocbag.prediction
# predict.bag.test
# test
remove(train,bagging.model,predict.bag.train,test.liftbag,
       train.rocbag,train.rocbag.prediction,train.liftbag)

save.image("bagging.RData")

#------------------#
# Confusion Matrix #  
#------------------#

load("bagging.RData")
# Using a p=.5 cutoff initially
predictions<-cut(predict.bag.test, c(-Inf,0.6,Inf), labels=c("Keep","Return"))
bagActuals <- factor(test$returnShipment,
                     levels = c(0,1),
                     labels = c("Keep", "Return"))
confusionMatrix(predictions, bagActuals)

# Get r squared
1 - sum((test$returnShipment-predict.bag.test)^2)/
  sum((test$returnShipment-mean(test$returnShipment))^2)
