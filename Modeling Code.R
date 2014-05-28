# Add Libraries
library(ROCR)
library(caret)
library(party) # for KT's Random Forest syntax

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

#------END TRAIN/TEST SPLIT-------#
# eliminate cases where timeToDeliver is NA 
# To test against produced LR model, need to do after train / test split, or else seed will select different observations
# Test shows zero difference in model coefficients - yay!
# Only (minor) differences in Final Model from initial run will be due to seed having a different placement in train/test regimen
# train <- train[complete.cases(train[,17]),]
# summary(train)


remove(orders.train) # To clean workspace for 'hungry' algorithms

### This is actually EDA, but wanted to do it after we had the train/test split.
# ROC curves for individual variables, using logistic regression
nm <- c("color","timeToDeliver","salutation","state",
  "accountAge","customerAge","holidayFlag","bdayFlag","LetterSize","Pants",
  "ChildSize","ShoeDress","difFromMeanPrice","price","numCustOrders",
  "numCustReturns","custRiskFlag","numItemReturns","numItemOrders",
  "itemRiskFlag","numManufOrders","numManufReturns","manufRiskFlag")
models <- lapply(nm, function(x) {
  glm(substitute(returnShipment ~ i, list(i = as.name(x))), 
      family=binomial(link=logit), data = train)
})

pdf("EDA_UnivariateROC.pdf",height=11,width=8.5)
for (i in seq(along = nm)) {
  predict.train.eda <- predict(models[[i]], train, type="response")
  predict.test.eda <- predict(models[[i]], test, type="response")
  
  train.eda.pred <- prediction(predict.train.eda, train$returnShipment)
  train.eda.roc <- performance(train.eda.pred, "tpr","fpr")
  train.eda.auc <- (performance(train.eda.pred, "auc"))@y.values
  
  test.eda.pred <- prediction(predict.test.eda, test$returnShipment)
  test.eda.roc <- performance(test.eda.pred, "tpr","fpr")
  test.eda.auc <- (performance(test.eda.pred, "auc"))@y.values
  
  # plot the model ROC curves
  plot(train.eda.roc, col = "darkgreen", 
       main = paste("ROC Curves for Logistic Regression Model\n",nm[i]))
  plot(test.eda.roc, col = "red",  add = TRUE)
  abline(c(0,1))
  # Draw a legend.
  train.legend <- paste("Train: AUC=", round(train.eda.auc[[1]], digits=3))
  test.legend <- paste("Test : AUC=", round(test.eda.auc[[1]], digits=3))
  legend("bottomright", c(train.legend,test.legend), fill=c("darkgreen","red"))
}
dev.off()
remove(i,models,nm,predict.test.eda,predict.train.eda,
      test.eda.auc,test.eda.pred,test.eda.roc,test.legend,
      train.eda.auc,train.eda.pred,train.eda.roc,train.legend)
gc()


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
                  + accountAge 
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

#---------------------------------------------------------------------#
#-----------------------END LOGISTIC REGRESSION-----------------------#
#---------------------------------------------------------------------#


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
# J48 cannot handle numeric class - have to convert to factor
RS <- as.factor(train$returnShipment)
returns_j48 <- J48(RS ~ color + timeToDeliver 
                   + salutation + state
                   + accountAge + customerAge 
                   + holidayFlag + bdayFlag 
                   + LetterSize + Pants + ChildSize + ShoeDress 
                   + difFromMeanPrice + price  
                   + numCustOrders + numCustReturns + custRiskFlag 
                   + numItemReturns + numItemOrders + itemRiskFlag
                   + numManufOrders + numManufReturns + manufRiskFlag,
                   data=train)
returns_j48
summary(returns_j48)


# to add a 10-folds cross-validation (does it help?)
eval_j48 <- evaluate_Weka_classifier(returns_j48, numFolds = 10, complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

remove(predict.test.J48, predict.train.J48, predictions, returns_j48, 
       test.logistic.auc, train.J48.pred, train.J48.roc,train.J48.auc,
       test.J48.pred, test.J48.roc, test.J48.auc,
       train.legend, test.legend, eval_j48)


#---SAME VARIABLES AS LR----#
# CANNOT DO a ROC Curve using ROCR if prediction is not a continuous variable #
# So it doesn't work with J48, which produces a discrete binary classification #
RS <- as.factor(train$returnShipment)
j48_selected <- J48(RS ~ color + timeToDeliver + salutation + 
                     accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
                     sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
                     numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
                     numItemOrders + numManufOrders,
                   data=train)
j48_selected
summary(j48_selected)


#-----------------------#
# And then a lift chart #
#-----------------------#
pdf("J48_Lift_Chart.pdf")
predict.train.J48 <- predict(returns_j48, train, type="probability")
predict.test.J48 <- predict(returns_j48, test, type="probability")

train.lift.J48 <- performance(predict.train.J48, "lift","rpp")
test.lift.J48 <- performance(predict.test.J48, "lift","rpp")
plot(train.lift.J48, col="green", main = "Lift Curve J48")
plot(test.lift.J48, col="red", add = TRUE)
legend("bottomleft",c("Sample","Test"),fill=(c("green","red")))
#legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()

remove(predict.test.J48, predict.train.J48, predictions, returns_j48, 
       test.logistic.auc, train.J48.pred, train.J48.roc,train.J48.auc,
       test.J48.pred, test.J48.roc, test.J48.auc,
       train.legend, test.legend, j48_selected, RS)



# PLACEHOLDER CODE FOR ROC
# WILL ROC WORK FOR J48? NO 
# Do we need to use type='class' or 'probability'?
# Keep getting error ' Format of labels is invalid.'
predict.train.J48 <- predict(returns_j48, train, type="response")
predict.test.J48 <- predict(returns_j48, test, type="response")

train.J48.pred <- prediction(predict.train.J48, train$RS)
train.J48.roc <- performance(train.J48.pred, "tpr","fpr")
train.J48.auc <- (performance(train.J48.pred, "auc"))@y.values

test.J48.pred <- prediction(predict.test.J48, test$RS)
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



#------------------------ END J48--------------------------------#

#--------------------#
#   Random Forests   #
#--------------------#
# KT - Code from 412 week 5, need to update to current dataset
# References for this section: 
# http://www.stanford.edu/~stephsus/R-randomforest-guide.pdf
# http://heuristically.wordpress.com/2009/12/18/plot-roc-curve-lift-chart-random-forest/

set.seed(498)
pdf("RandomForestPlots.pdf")

# The code crashed RStudio on me- trying with a much smaller sample to see if the syntax itself works
sample_ind <- sample(seq_len(nrow(train)), size = 1000)
train.sample <- train [sample_ind, ]

# fit a random forest model to smaller training set
data.controls <- cforest_unbiased(ntree=1000, mtry=5) #ntree should be increased from default of 500 based on number of predictors and datapoints, mtry default is 5, suggested is sqrt of predictors
cforest.model <- cforest(returnShipment ~ color + timeToDeliver 
                         + salutation + state
                         + accountAge + customerAge 
                         + holidayFlag + bdayFlag 
                         + LetterSize + Pants + ChildSize + ShoeDress 
                         + difFromMeanPrice + price  
                         + numCustOrders + numCustReturns + custRiskFlag 
                         + numItemReturns + numItemOrders + itemRiskFlag
                         + numManufOrders + numManufReturns + manufRiskFlag
                         , data = train.sample, controls=data.controls) 

# Variable importance - note this can also be done using randomForest as the library, but produces a dot plot
data.cforest.varimp <- varimp(cforest.model)
barplot(sort(data.cforest.varimp), horiz=T, xlab="Variable Importance in Training (vars to the rt of dotted line are sig)",las=1,cex.names=0.5)
abline(v=mean(data.cforest.varimp), col="red",lty="longdash", lwd=2)
abline(v=median(data.cforest.varimp), col="blue", lwd=2)
abline(v=abs(min(data.cforest.varimp)),col="black",lty="dotted",lwd=2)
legend("bottomright",c("Mean","Median","| Min |"),lty=c("longdash","solid","dotted"),col=c("red","blue","black"),lwd=c(2,2,2))
# based on sample & settings, most important vars are itemRisk, numItems, and manufRisk
# the model doesn't appear stable though- the rest of the variables move around in importance

# Use the model to predict.
predict.forest.sample <- predict(cforest.model)
#predict.forest.train <- predict(cforest.model)
predict.forest.test <- predict(cforest.model, newdata = test)

# Borrowed CM syntax from LR above
RFpredictions<-cut(predict.forest.sample, c(-Inf,0.5,Inf), labels=c("Keep","Return"))
str(RFpredictions)
summary(RFpredictions)
RFactuals <- factor(train.sample$returnShipment,
                    levels = c(0,1),
                    labels = c("Keep", "Return"))
confusionMatrix(RFpredictions,RFactuals)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
#detach("package:neuralnet", unload=TRUE) # the prediction function of ROCR was getting overwritten
sample.rocforest.prediction <- prediction(predict.forest.sample, train.sample[['returnShipment']])
#train.rocforest.prediction <- prediction(predict.forest.train, train$returnShipment)
test.rocforest.prediction <- prediction(predict.forest.test, test$returnShipment)

sample.rocforest <- performance(sample.rocforest.prediction, "tpr","fpr")
#train.rocforest <- performance(train.rocforest.prediction, "tpr","fpr")
test.rocforest <- performance(test.rocforest.prediction, "tpr","fpr")

plot(sample.rocforest, col="green", main = "ROC Random Forest")
#plot(train.rocforest, col="blue", main = "ROC Random Forest")
plot(test.rocforest, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Sample: AUC ="
        ,round(as.numeric(performance(sample.rocforest.prediction,"auc")@y.values),4))
        ,paste("Test: AUC ="
        ,round(as.numeric(performance(test.rocforest.prediction,"auc")@y.values),4)))
        ,fill=(c("green","red")))
#legend("bottomright",c(paste("Training: AUC =",round(as.numeric(performance(train.rocforest.prediction,"auc")@y.values),4)),paste("Test: AUC =",round(as.numeric(performance(test.rocforest.prediction,"auc")@y.values),4))),fill=(c("blue","red")))

# And then a lift chart
sample.liftforest <- performance(sample.rocforest.prediction, "lift","rpp")
#train.liftforest <- performance(train.rocforest.prediction, "lift","rpp")
test.liftforest <- performance(test.rocforest.prediction, "lift","rpp")
plot(sample.liftforest, col="green", main = "Lift Curve Random Forest")
#plot(train.liftforest, col="blue", main = "Lift Curve Random Forest")
plot(test.liftforest, col="red", add = TRUE)
legend("bottomleft",c("Sample","Test"),fill=(c("green","red")))
#legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()

#clean up everything other than the model itself & test.rocforest/test.rocforest.prediction
remove(predict.forest.sample,predict.forest.test,data.cforest.varimp,data.controls,sample.liftforest,sample.rocforest
       ,sample.rocforest.prediction,sample_ind,test.liftforest)
#clean up everything other than the model itself & test.rocforest
remove(predict.forest.sample,predict.forest.test,data.cforest.varimp,data.controls,sample.liftforest,sample.rocforest
       ,sample.rocforest.prediction,sample_ind,test.liftforest,RFactuals,RFpredictions,test.rocforest.prediction)
gc()
memory.size()
memory.limit()

# fit a random forest model to smaller training set with fewer variables, based on var imp plot
data.controls <- cforest_unbiased(ntree=1000, mtry=3) #ntree should be increased from default of 500 based on number of predictors and datapoints, mtry default is 5, suggested is sqrt of predictors
cforest.model <- cforest(returnShipment ~ numCustReturns + numItemReturns + numManufReturns
                         + difFromMeanPrice + price
                         + numCustOrders + timeToDeliver + LetterSize
                         , data = train.sample, controls=data.controls) 
# Use the model to predict.
predict2.forest.sample <- predict(cforest.model)
predict2.forest.test <- predict(cforest.model, newdata = test)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
sample.rocforest.prediction2 <- prediction(predict2.forest.sample, train.sample[['returnShipment']])
test.rocforest.prediction2 <- prediction(predict2.forest.test, test$returnShipment)

sample.rocforest2 <- performance(sample.rocforest.prediction2, "tpr","fpr")
test.rocforest2 <- performance(test.rocforest.prediction2, "tpr","fpr")

plot(sample.rocforest2, col="green", main = "ROC Random Forest")
plot(test.rocforest2, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Sample: AUC ="
                             ,round(as.numeric(performance(sample.rocforest.prediction2,"auc")@y.values),4))
                       ,paste("Test: AUC ="
                              ,round(as.numeric(performance(test.rocforest.prediction2,"auc")@y.values),4)))
       ,fill=(c("green","red")))

# reload the neuralnet package so it's available for later syntax
library(neuralnet)

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

# KT - This is my code from 412 if it helps, delete if it doesn't!
################ SVM Model ###################
# Reference:
# http://heuristically.wordpress.com/2009/12/23/compare-performance-machine-learning-classifiers-r/

pdf("SVM.pdf")

# svm requires tuning
set.seed(498)
x.svm.tune <- tune(svm, class~., data = working.train, ranges = list(gamma = 2^(-12:1), cost = 2^(0:8)), tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
x.svm.tune

# fit an SVM model to training set
# Manually copy the cost and gamma from console messages above to parameters below.
svm.model <- svm(class ~ ., data = working.train, cost=256, gamma=0.0002441406, probability = TRUE)

# Use the model to predict
predict.svm.train <- predict(svm.model, working.train, probability = TRUE)
predict.svm.test <- predict(svm.model, newdata = working.test, probability = TRUE)

# Plot the performance of the model applied to the evaluation set as an ROC curve.
train.rocsvm.prediction <- prediction(attr(predict.svm.train,"probabilities")[,1], working.train$class)
test.rocsvm.prediction <- prediction(attr(predict.svm.test, "probabilities")[,1], working.test$class)
train.rocsvm <- performance(train.rocsvm.prediction, "tpr","fpr")
test.rocsvm <- performance(test.rocsvm.prediction, "tpr","fpr")
plot(train.rocsvm, col="blue", main = "ROC SVM")
plot(test.rocsvm, col="red", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Training: AUC =",round(as.numeric(performance(train.rocsvm.prediction,"auc")@y.values),4)),paste("Test: AUC =",round(as.numeric(performance(test.rocsvm.prediction,"auc")@y.values),4))),fill=(c("blue","red")))

# And then a lift chart
train.liftsvm <- performance(train.rocsvm.prediction, "lift","rpp")
test.liftsvm <- performance(test.rocsvm.prediction, "lift","rpp")
plot(train.liftsvm, col="blue", main = "Lift Curve SVM")
plot(test.liftsvm, col="red", add = TRUE)
legend("bottomleft",c("Training","Test"),fill=(c("blue","red")))
dev.off()

#---------------------------------#
#   Artifical Neural Networks    #
#                                 #
#             aka                 #
#                                 #
#   Associative Neural Networks   #
#   (depending on how well I'm    #
#   proofreading my contributions)#
#---------------------------------#

library(neuralnet)

set.seed(2000)

#Need to convery all our factors to quantitative inputs using dummy variables
# R doesn't do this for you -- see http://stackoverflow.com/questions/17457028/working-with-neuralnet-in-r-for-the-first-time-get-requires-numeric-complex-ma
simpleDesignMatrix <- model.matrix( ~ returnShipment + timeToDeliver, 
                                    data = orders.train )

nnSimple <- neuralnet(returnShipment ~ timeToDeliver,
                   data = simpleDesignMatrix, hidden=1, threshold=0.01,
                   linear.output = FALSE, likelihood = TRUE )
out <- nnSimple$net.result[[1]]
head(out)
#Don't need the intercept for this
simpleTestMatrix <- simpleDesignMatrix[,3]

simpleResults <- compute(nnSimple, simpleTestMatrix)
simplePredictions <- predict(nnSimple, simpleTestMatrix, type = "class")
summary(simpleResults$net.result)
#Full model




################ ANN Model ###################
library(neuralnet)


formula = returnShipment ~ color + timeToDeliver + salutation + 
  accountAge + holidayFlag + LetterSize + ChildSize + ShoeDress + 
  sizeHighRisk + sizeLowRisk + difFromMeanPrice + price + numItemsInOrder + 
  numCustOrders + numCustReturns + custRiskFlag + numItemReturns + 
  numItemOrders + numManufOrders


designMatrix <- model.matrix(formula, data = train)

covList = c("color", "timeToDeliver", "salutation", 
              "accountAge", "holidayFlag", "LetterSize", "ChildSize", "ShoeDress", 
              "sizeHighRisk",  "sizeLowRisk", "difFromMeanPrice", "price", "numItemsInOrder", 
              "numCustOrders",  "numCustReturns", "custRiskFlag",  "numItemReturns",
              "numItemOrders",  "numManufOrders")
#Need to fix the column names so we can use them ase variable inputs into our formula
colnames(designMatrix)[18] <- "salutationnotreported"
#colnames(designMatrix)[26] <- "stateLowerSaxony"
#colnames(designMatrix)[27] <- "stateMecklenburgWesternPomerania"
#colnames(designMatrix)[28] <- "stateNorthRhineWestphalia"
#colnames(designMatrix)[29] <- "stateRhinelandPalatinate"
#colnames(designMatrix)[32] <- "stateSaxonyAnhalt"
#colnames(designMatrix)[33] <- "stateSchleswigHolstein"

nnData = cbind(train$returnShipment,designMatrix, deparse.level = 2)[1:10000,]
colnames(nnData)[1] <- "returnShipment"
nnformula <- as.formula(paste("returnShipment ~ ", paste(colnames(designMatrix[,-1]),collapse ="+")))
print(paste("Start training at: ", Sys.time()))
nn <- neuralnet( nnformula, data = nnData, hidden=1, threshold=0.01,
                linear.output = FALSE, likelihood = TRUE )
print(paste("Stop training at: ", Sys.time()))
print(nn$net.result)
testDesignMatrix <- model.matrix(formula, data = test)
colnames(testDesignMatrix)[18] <- "salutationnotreported"
covariate <- subset( testDesignMatrix, select = nn$model.list$variables)

testResults <- compute(nn, covariate)
summary(simpleResults$net.result)

#Generate our test statistics for the neuralnet

predict.test.nn <- ROCR::prediction(predictions=testResults$net.result, labels=test[,"returnShipment"]) #, test) #, type="response")

#test.nn.pred <- prediction(predict.train.eda, train$returnShipment)
test.nn.roc <- performance(predict.test.nn, "tpr","fpr")
test.nn.auc <- (performance(predict.test.nn, "auc"))@y.values

#----------------------#
#   Model Comparison   #
#----------------------#

# Code below is placeholder from 412 and needs to be updated
################ All Models in one ROC (test data only) ###################
pdf("ModelComparison.pdf")
plot(test.logistic.roc, col="blue", main = "ROC Model Comparison")

plot(test.rocforest, col="red", add = TRUE)
plot(test.rocsvm, col="green", add = TRUE)
plot(test.rocann, col="grey", add = TRUE)
abline(c(0,1))
legend("bottomright",c(paste("Logistic: AUC ="
    ,round(as.numeric(performance(test.logistic.pred,"auc")@y.values),4))
    ,paste("Random Forest: AUC ="
    ,round(as.numeric(performance(test.rocforest.prediction,"auc")@y.values),4))
    ,paste("SVM: AUC ="
    ,round(as.numeric(performance(test.rocsvm.prediction,"auc")@y.values),4))
    ,paste("ANN: AUC ="
    ,round(as.numeric(performance(test.rocann.prediction,"auc")@y.values),4)))
    ,fill=(c("blue","red","green","grey")))
dev.off()

################ All Models - Numeric Comparisons ###################
R <- cor(cbind(trainTr$medvTr, fitted(bostonTr.model), fitted(bostonTr.step), predict(bostonTr.tree),predict(bostonTr10.nnet,trainTr)*(max(bostonTr$medvTr)-min(bostonTr$medvTr))*min(bostonTr$medvTr)))
rownames(R) <- colnames(R) <- c("Actual Values","Transformed","Tr Stepwise","Tr Tree","Tr Network")
R

Rtest <- cor(cbind(testTr$medvTr, predict(bostonTr.model,testTr), predict(bostonTr.step,testTr), predict(bostonTr.tree,testTr),predict(bostonTr10.nnet,testTr)*(max(bostonTr$medvTr)-min(bostonTr$medvTr))*min(bostonTr$medvTr)))
rownames(Rtest) <- colnames(Rtest) <- c("Actual Values","Transformed","Tr Stepwise","Tr Tree","Tr Network")
Rtest

############### All models ---- AIC / BIC ###############################
aic.lr <- AIC(returns.lr)
bic.lr <- BIC(returns.lr)
aic.j48 <- AIC(returns_j48)
bic.j48 <- BIC(returns_j48)
aic.rf <- AIC(cforest.model) # doesn't work for RF
bic.rf <- BIC(cforest.model) # same



rmse <- function(observed,predicted) {
  sqrt(mean((observed-predicted)^2))
}
c(FullOLS<-rmse(testTr$medvTr,predict(bostonTr.model,testTr)),SubsetOLS<-rmse(testTr$medvTr,predict(bostonTr.step,testTr)),Tree<-rmse(testTr$medvTr,predict(bostonTr.tree,testTr)),NNet<-rmse(testTr$medvTr,predict(bostonTr10.nnet,testTr)*(max(bostonTr$medvTr)-min(bostonTr$medvTr))*min(bostonTr$medvTr)))