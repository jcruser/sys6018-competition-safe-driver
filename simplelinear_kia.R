library(readr)
library (randomForest)
library(ranger)

#Read in data
setwd("~/Desktop/SYS 6018/Kaggle competitions/safe driver")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Treat the categorical vars as factors
cats <- names(train)[grepl('_cat$', colnames(train))]
bins <- names(train)[grepl('_bin$', colnames(train))]
apply(train[,cats],2,as.factor)
apply(train[,bins],2,as.factor)
cats <- names(test)[grepl('_cat$', colnames(test))]
bins <- names(test)[grepl('_bin$', colnames(test))]
apply(test[,cats],2,as.factor)
apply(test[,bins],2,as.factor)

#Treat missing values
train[train==-1] <- -1
test[test==-1] <- -1


train.sample <- train[sample(1:nrow(train),size=1000),]


###### Logistic Regression ######
glm <- glm(target~.-id,data=train,family="binomial")
sig_vars <- summary(glm)$coeff[-1,4] < 0.05
sig_vars <- names(sig_vars)[sig_vars == TRUE] 
sig_formula <- as.formula(paste("target ~",sig_vars))
glm_sig <- glm(formula=sig_formula,data=train,family="binomial")
probs <- predict(glm, newdata = test, type = "response")
preds <- rep(0,892816)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
mypreds <- cbind(as.character(test$id),preds)

#Write out the predictions to a csv file
write.table(mypreds, file = "submission1.csv", row.names=F, col.names=c("id","target"), sep=",")

###### Random Forest######
RF1 <- ranger(target~.-id,data=train,write.forest=T)
predsRF1<-predict(RF1,test)
preds <- rep(0,892816)  # Initialize prediction vector
preds[predsRF1$predictions>0.1] <- 1 # p>0.5 -> 1
mypredsRF1 <- cbind(as.character(test$id),preds)

#Write out the predictions to a csv file
write.table(mypredsRF1, file = "submissionRF1.csv", row.names=F, col.names=c("id","target"), sep=",")











##### Gini #####
#' Calculates unnormalized Gini index from ground truth and predicted probabilities.
#' @param ground.truth Ground-truth scalar values (e.g., 0 and 1)
#' @param predicted.probabilities Predicted probabilities for the items listed in ground.truth
#' @return Unnormalized Gini index.
unnormalized.gini.index = function(ground.truth, predicted.probabilities) {
  
  if (length(ground.truth) !=  length(predicted.probabilities))
  {
    stop("Actual and Predicted need to be equal lengths!")
  }
  
  # arrange data into table with columns of index, predicted values, and actual values
  gini.table = data.frame(index = c(1:length(ground.truth)), predicted.probabilities, ground.truth)
  
  # sort rows in decreasing order of the predicted values, breaking ties according to the index
  gini.table = gini.table[order(-gini.table$predicted.probabilities, gini.table$index), ]
  
  # get the per-row increment for positives accumulated by the model 
  num.ground.truth.positivies = sum(gini.table$ground.truth)
  model.percentage.positives.accumulated = gini.table$ground.truth / num.ground.truth.positivies
  
  # get the per-row increment for positives accumulated by a random guess
  random.guess.percentage.positives.accumulated = 1 / nrow(gini.table)
  
  # calculate gini index
  gini.sum = cumsum(model.percentage.positives.accumulated - random.guess.percentage.positives.accumulated)
  gini.index = sum(gini.sum) / nrow(gini.table) 
  return(gini.index)
}

#' Calculates normalized Gini index from ground truth and predicted probabilities.
#' @param ground.truth Ground-truth scalar values (e.g., 0 and 1)
#' @param predicted.probabilities Predicted probabilities for the items listed in ground.truth
#' @return Normalized Gini index, accounting for theoretical optimal.
normalized.gini.index = function(ground.truth, predicted.probabilities) {
  
  model.gini.index = unnormalized.gini.index(ground.truth, predicted.probabilities)
  optimal.gini.index = unnormalized.gini(ground.truth, ground.truth)
  return(model.gini.index / optimal.gini.index)
}
