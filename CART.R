stevens=read.csv("stevens.csv")
head(stevens,5)
str(stevens)
# load all packages
library(caTools)
library(ROCR)
library(mice)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
library(caret)
library(e1071)
library(rattle)
library(RColorBrewer)
library(Amelia)
# let us split into trainig and test datasets
set.seed(3000)
spl=sample.split(stevens$Reverse,0.70)
train=subset(stevens,spl==TRUE)
test=subset(stevens,spl==FALSE)

# let us build the model

stevensTree=rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="class",minbucket=25)

#  minbucket puts a lower bound on the number of data points in each bucket
#  method="class" is always used while building a tree
# instead of a regression problem. If we omit this we get the probabilities.
windows()
prp(stevensTree)   # to plot the tree

#lets make prediction for the test set
predictCART=predict(stevensTree,newdata=test,type="class")
#  type="class" is Ommited to get the probabilities
# lets check the confusion matrix
table(test$Reverse,predictCART)
#0.659 accuracy

# let us check the ROC curve to see if a threshold of 0.5 is good

predictCART=predict(stevensTree,newdata=test)  # prediction to find the probabilities

ROCR=prediction(predictCART[,2],test$Reverse)
ROCCurve=performance(ROCR,"tpr","fpr")
plot(ROCCurve,colorize=T)
# The ROC curve shows that 0.6 will be a good threshold value

#**************************
#RANDOM FOREST
#**************************

# here a large no. of CARt trees is built hence the model becomes less interpretable

# to make prediction for a new outcome each tree votes on the outcome and we
# pick the outcome that receives the maximum no. of votes

# each tree built in random forest is built by Bootstrap/ bagged method
# i.e by selecting a random sample (with replacement) of the independent variables for trainig data.
# since each tree sees a different data each of the trees built is different. hence as a result
# we get a forest of many different trees.

stevensForest1=randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train, nodesize=25,ntree=200)

# nodesize= is similar to minbucket in cart
# ntree is the no. of trees to build
# in random forest no method="class" is used. hence if we want to do a classification problem
# we have to make sure that the outcome variable is a Factor else it will be a Regression 
#problem and it will generate the probabilities.

train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)

stevensForest2=randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train, nodesize=25,ntree=200)

rfpred1=predict(stevensForest2,newdata=test)

rfpred2=predict(stevensForest1,newdata=test) # the output of rfpred2 will be probabilities
# table(test$Reverse,rfpred2>0.5)  hence this has to be used if we are taking stevensForest1 

table(test$Reverse,rfpred1)
# accuracy 0.694



#************************************ K folds cross validation*********************************

# first split the training set into k equal subsets or folds. then use k-1 folds to estimate the model 
# and compute predictions on the remianign 1 fold (validation fold). We bulid a model and make 
# predictions for each possible parameter value we are considering. We repeat for each folds.


#The for each candidate parameter value and each fold we plot the accuracy of the models.
# Then take the average accuracy with respect to the parameter and take the final parameter which 
#gives maximum accuracy.  This parameter is called cp (complexity parameter) its like R2 and AIC (logistic regression)

numfolds=trainControl(method="cv",number=10) # cv= cross validation, 10 folds
cpgrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="rpart",trControl=numfolds,tuneGrid=cpgrid)
# here we used method="rpart" since we are cross validating a cart model

#The final value used for the model was cp = 0.19. 

# lets use this cp value instead of the minbucket parameter


stevensTreeCV=rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="class",cp=0.19)

predictCV=predict(stevensTreeCV,newdata=test,type="class")
table(test$Reverse,predictCV)
# this gives a accuracy is 0.724 far greater than previous CART model









































