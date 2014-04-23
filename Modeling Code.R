# Models and ML Algorithms

summary(orders.train)

#----------------------#
#  Logistic Regression #
#----------------------#
# Look at Week 3 assignment of Predict 412

# LR "Full Model"
returns.lr <- glm(returnShipment ~ color + timeToDeliver + accountAge 
                  + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                  + manufRiskFlag + itemRiskFlag,
              family=binomial(link=logit), data=orders.train)
summary(returns.lr)

# Backwards elimination selection
# use default AIC measure
# eliminates variables X1 and X2....  
# Note step function uses full model defined above 
returns.backward <- step(returns.lr)
summary(returns.backward)


# TO Get ROC Curves
# get predictions from model 
# NEED TO KNOW WHAT WE CALL TRAIN AND TEST DATA SETS
predict.train.logistic <- predict(returns.lr, orders.train, type="response")
predict.test.logistic <- predict(returns.lr, orders.test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, orders.train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the full model ROC curves
pdf(file = "full_model_ROC.pdf", width = 11, height = 8.5)  ##/\open pdf/\##
plot(train.logistic.roc, col = "darkgreen", main = "ROC Curves for Logistic Regression Model")
plot(test.logistic.roc, col = "red",  add = TRUE)
abline(c(0,1))
# Draw a legend.
train.legend <- paste("Train: AUC=", round(train.logistic.auc[[1]], digits=3))
test.legend <- paste("Test : AUC=", round(test.logistic.auc[[1]], digits=3))
legend(0.6, 0.5, c(train.legend,test.legend), c(3,2))


#------------------#
# Confusion Matrix #  
#------------------#
# We need to convert preds to actual choice; introduce 'cut'
# selected a p=.65 cutoff after review of ROC
predictions<-cut(rpredict.test.logistic, c(-Inf,0.65,Inf), labels=c("ATT","OCC"))
# Now have a look - classes are assigned
str(predictions)
summary(predictions)
# compare to test$pick to ensure same # of levels and obs
str(test$pick)
summary(test$pick)

confusionMatrix(predictions, test$pick)



#------------------#
#  Decision Trees  #
#------------------#
# J48 (based on Quinlan's C4.5)
library(RWeka)
# to run j48 in RWeka
# Get an error that j48 cannot handle numeric class - do we have to convert to something else for this to work?
returns_j48 <- J48(returnShipment ~ color + timeToDeliver + accountAge 
                     + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                     + manufRiskFlag + itemRiskFlag
                     , data = orders.train)
returns_j48
summary(returns_j48)


#--------------------#
#   Random Forests   #
#--------------------#
# Get strange error message 

library(randomForest)
fit <- randomForest(returnShipment ~ color + timeToDeliver + accountAge 
                    + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                    + manufRiskFlag + itemRiskFlag
                      , data = orders.train)
print(fit) # view results
importance(fit) # importance of each predictor 
# we note variable x1 is most important, followed by x2, x3, and x4

RFprediction <- predict(fit, test)
confusionMatrix(RFprediction, test$pick)

#-----------------------------#
#   Support Vector Machines   #
#-----------------------------#



# Ensemble Methods
# Perhaps can average prediction from some of above