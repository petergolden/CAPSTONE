#Final Logistic Model

# Add Libraries
library(ROCR)
library(caret)
library(party) 

# Load orders.train post imputation and transformations
# load("~/CAPSTONE/imputedOrdersPostTransformation.rdata")

# Models and ML Algorithms


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

remove(orders.train) # To clean workspace for 'hungry' algorithms
#------END TRAIN/TEST SPLIT-------#

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
pdf(file = "BE_LR_Model_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##

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


rmse <- function(observed,predicted) {
  sqrt(mean((observed-predicted)^2))
}

rmse(test$returnShipment,predict.test.logistic) # 0.396 - Also Ouch!

# clean up everything except stuff for combined ROC/rmse:
# test
# predict.test.logistic
# test.logistic.pred
# test.logistic.roc


remove(train, BE.LR.Model, predict.train.logistic, test.logistic.auc, train.logistic.auc, train.logistic.pred, train.logistic.roc)

# save workspace
save.image(file="Logistic_rmse_ROC_space.RData")

