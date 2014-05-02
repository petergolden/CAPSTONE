# Add Libraries
library(ROCR)
library(caret)
# Models and ML Algorithms

summary(orders.train)

#----------------------#
#  Logistic Regression #
#----------------------#
# Look at Week 3 assignment of Predict 412

# LR "Full Model"
returns.lr <- glm(returnShipment ~ color + timeToDeliver + accountAge 
                  + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                  + manufRiskFlag + itemRiskFlag + custRiskFlag 
                  + LetterSize + Pants + ChildSize + ShoeDress + difFromMeanPrice + state
                  + price + salutation + customerAge + accountAge,
              family=binomial(link=logit), data=train)
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
predict.train.logistic <- predict(returns.lr, train, type="response")
predict.test.logistic <- predict(returns.lr, test, type="response")

train.logistic.pred <- prediction(predict.train.logistic, train$returnShipment)
train.logistic.roc <- performance(train.logistic.pred, "tpr","fpr")
train.logistic.auc <- (performance(train.logistic.pred, "auc"))@y.values

test.logistic.pred <- prediction(predict.test.logistic, test$returnShipment)
test.logistic.roc <- performance(test.logistic.pred, "tpr","fpr")
test.logistic.auc <- (performance(test.logistic.pred, "auc"))@y.values

# plot the full model ROC curves
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

#------------------#
# Confusion Matrix #  
#------------------#
# We need to convert preds to actual choice; introduce 'cut'
# selected a p=.4 cutoff after review of ROC
predictions<-cut(predict.test.logistic, c(-Inf,0.4,Inf), labels=c("Keep","Return"))
# Now have a look - classes are assigned
str(predictions)
summary(predictions)
# compare to test$pick to ensure same # of levels and obs
# Need to impute or eliminate observations with NAs or else have above issue
str(test$returnShipment)
summary(test$returnShipment)

confusionMatrix(predictions, test$returnShipment)



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
                     , data = train)
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
                      , data = train)
print(fit) # view results
importance(fit) # importance of each predictor 
# we note variable x1 is most important, followed by x2, x3, and x4

RFprediction <- predict(fit, test)
confusionMatrix(RFprediction, test$returnShipment)

#-----------------------------#
#   Support Vector Machines   #
#-----------------------------#
library(e1071)  	#for Support Vector Machines


#------PLACE HOLDER FROM 412 CODE------------#

# Whoa there!  This one killed my PC! (JB)  
# Not sure if that's b/c of the NA's or if it's an expensive computational method


svmmodel <- svm(returnShipment ~ color + timeToDeliver + accountAge 
                + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                + manufRiskFlag + itemRiskFlag
                , data = train)
print(svmmodel)
summary(svmmodel)

svmprediction <- predict(svmmodel, test)
confusionMatrix(svmprediction, test$returnShipment)
ca <- table(svmprediction, test$returnShipment)
classAgreement(ca)

# optimize C and Gamma
tobj <- tune.svm(returnShipment ~ color + timeToDeliver + accountAge 
                 + customerAge + holidayFlag + bdayFlag + numItemsInOrder
                 + manufRiskFlag + itemRiskFlag
                 , data = train, 
                 gamma = 10^(-6:-3), cost = 10^(1:2))
summary(tobj)

#plot error landscape
pdf(file = "SVM_error_landscape.pdf", width = 11, height = 8.5)	##/\open pdf/\##
plot(tobj, transform.x = log10, xlab = expression(log[10](gamma)),
     ylab = "C")
dev.off()										##\/close pdf\/##


# use optimized  C and gamma
bestGamma <- tobj$best.parameters[[1]]
bestC <- tobj$best.parameters[[2]]
newsvmmodel <- svm(returnShipment ~ ., data = train,
                   cost = bestC, gamma = bestGamma, cross = 10)
summary(newsvmmodel)

# show confusion matrix
newsvmprediction <- predict(newsvmmodel, test)
confusionMatrix(newsvmprediction, test$returnShipment)

# show class agreement function
ca <- table(newsvmprediction, test$returnShipment)
classAgreement(ca)


# Ensemble Methods
# Perhaps can average prediction from some of above