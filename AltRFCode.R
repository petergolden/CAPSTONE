# Add Libraries
library(ROCR)
library(caret)
library(party) # for KT's Random Forest syntax

load("imputedOrdersPostTransformation.rdata")

#eliminate cases where timeToDeliver is NA
orders.train <- orders.train[complete.cases(orders.train[,17]),]
# Was having trouble with the smaller subset of variables
# creating an even smaller dataset by only having variables used by the model
# (based on var imp plot)
orders <- cbind(orders.train[9],orders.train[14],orders.train[17],
                orders.train[25],orders.train[40],orders.train[43],
                orders.train[45:46],orders.train[54],orders.train[55])
str(orders)

#train/test split, using sample of only 1000 orders for the train set
set.seed(498)
train_ind <- sample(seq_len(max(orders.train$orderID)), size = 1000)
trainTest <- train_ind[orders$orderID]
train2 <- orders[which(trainTest>0), ]
test2 <- orders[-which(trainTest>0), ]
remove(trainTest,train_ind,orders.train,orders)
gc()

# fit a random forest model to smaller training set with fewer variables, based on var imp plot
data.controls <- cforest_unbiased(ntree=1000, mtry=3) #ntree should be increased from default of 500 based on number of predictors and datapoints, mtry default is 5, suggested is sqrt of predictors
cforest.model <- cforest(returnShipment ~ numCustReturns + numItemReturns + numManufReturns
                         + difFromMeanPrice + price
                         + numCustOrders + timeToDeliver + LetterSize
                         , data = train2, controls=data.controls) 
# Use the model to predict.
predict2.forest.sample <- predict(cforest.model)
# ran out of memory trying to do whole test set, splitting
summary(test2$orderID)
test2.1 <- test2[which(test2$orderID<34770), ]
test2.2 <- test2[which(test2$orderID>=34770 & test2$orderID<67750), ]
test2.3 <- test2[which(test2$orderID>=67750 & test2$orderID<102400), ]
test2.4 <- test2[which(test2$orderID>=102400), ]
remove(test2)

predict2.forest.test1 <- predict(cforest.model, newdata = test2.1)
predict2.forest.test2 <- predict(cforest.model, newdata = test2.2)
predict2.forest.test3 <- predict(cforest.model, newdata = test2.3)
predict2.forest.test4 <- predict(cforest.model, newdata = test2.4)

#merge
predict2.forest.test <- rbind(predict2.forest.test1,predict2.forest.test2,
                              predict2.forest.test3,predict2.forest.test4)

actuals.test1 <- as.data.frame(test2.1$returnShipment)
actuals.test2 <- as.data.frame(test2.2$returnShipment)
actuals.test3 <- as.data.frame(test2.3$returnShipment)
actuals.test4 <- as.data.frame(test2.4$returnShipment)
colnames(actuals.test1) <- "returnShipment"
colnames(actuals.test2) <- "returnShipment"
colnames(actuals.test3) <- "returnShipment"
colnames(actuals.test4) <- "returnShipment"
actuals.test <- rbind(actuals.test1,actuals.test2,
                      actuals.test3,actuals.test4)

remove(actuals.test1,actuals.test2,actuals.test3,actuals.test4)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
sample.rocforest.prediction2 <- prediction(predict2.forest.sample, train2$returnShipment)
test.rocforest.prediction2 <- prediction(predict2.forest.test, actuals.test$returnShipment)

sample.rocforest2 <- performance(sample.rocforest.prediction2, "tpr","fpr")
test.rocforest2 <- performance(test.rocforest.prediction2, "tpr","fpr")

pdf("RF_SubsetByVarImp_ROC.pdf")
plot(sample.rocforest2, col="green", main = "ROC Random Forest")
plot(test.rocforest2, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Sample: AUC ="
                             ,round(as.numeric(performance(sample.rocforest.prediction2,"auc")@y.values),4))
                       ,paste("Test: AUC ="
                              ,round(as.numeric(performance(test.rocforest.prediction2,"auc")@y.values),4)))
       ,fill=(c("green","red")))
dev.off()

# clean up everything except model itself and test.rocforest
remove(actuals.test,predict2.forest.sample,predict2.forest.test,
       predict2.forest.test1,predict2.forest.test2,predict2.forest.test3,
       predict2.forest.test4,test2.1,test2.2,test2.3,test2.4,train2,
       data.controls,sample.rocforest.prediction2,sample.rocforest2)

# save workspace
save.image(file="RF_workspace.rdata")
