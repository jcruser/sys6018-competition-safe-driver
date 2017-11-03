library(readr)
library (randomForest)
library(ranger)
library(leaps)


#Read in data
setwd("~/Desktop/SYS 6018/Kaggle competitions/safe driver")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#####################################
#  First Try: Logistic Regression   #
#####################################

glm <- glm(target~.-id,data=train,family="binomial")
sig_vars <- summary(glm)$coeff[-1,4] < 0.05
sig_vars <- names(sig_vars)[sig_vars == TRUE] 
sig_formula <- as.formula(paste("target ~",sig_vars))
glm_sig <- glm(formula=sig_formula,data=train,family="binomial")
probs <- predict(glm, newdata = test, type = "response")
mypreds <- cbind(as.character(test$id),probs)

#Write out the predictions to a csv file
write.table(mypreds, file = "submission1.csv", row.names=F, col.names=c("id","target"), sep=",")

#####################################
#    Data cleaning & Exploration    #
#####################################

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
#Calculate number of missing values in each column
Missings <- apply(train,2,FUN=function(x) length(which(x==-1)))
Missings
#ps_car_03_cat, ps_car_05_cat and ps_reg_03 are missing too many values
dropvar <- c("ps_car_03_cat","ps_car_05_cat","ps_reg_03")
train.clean <- train[,-which(names(train) %in% dropvar)]
Missings <- apply(train.clean,2,FUN=function(x) length(which(x==-1)))
Missings
#Impute missing values for ps_car_14,ps_car_07_cat and ps_ind_05_cat
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
train.clean[train.clean$ps_car_14==-1,]$ps_car_14 = mean(train.clean[train.clean$ps_car_14!=-1,]$ps_car_14)
train.clean[train.clean$ps_ind_05_cat==-1,]$ps_ind_05_cat = Mode(train.clean[train.clean$ps_ind_05_cat!=-1,]$ps_ind_05_cat)
train.clean[train.clean$ps_car_07_cat==-1,]$ps_car_07_cat = Mode(train.clean[train.clean$ps_car_07_cat!=-1,]$ps_car_07_cat)

#For computational power limits reasons, sample the training set
train.sample <- train[sample(1:nrow(train),size=1000),]
nrow(train[train$target==1,])
nrow(train[train$target==0,])
#Because the training set is inbalanced, make a balanced sample
#with 50% positives(1) and 50% negatives(0)
positives <- train.clean[train$target==1,]
negatives <- train.clean[train$target==0,]
set.seed(123)
train.sample <- rbind(positives,negatives[sample(1:nrow(negatives),size=21694),])
#Make a random validation set for model evaluation
valid.sample <- train[sample(1:nrow(train),size=5000),]
valid.sample.truth <- valid.sample$target
valid.sample$target <- NULL

#####################################
#         Stepwise: Log Reg         #
#####################################

glm.null <- glm(target~1, data=train.sample,family="binomial")
glm.full <- glm(target~.-id, data=train.sample,family="binomial")
#Stepwise selection with the training sample set
step(glm.null, scope=list(lower=glm.null, upper=glm.full), direction="both")
#Final formula:
#Use the full training set on the final model
glm.step <-  glm(formula = target ~ ps_car_13 + ps_ind_17_bin + ps_ind_05_cat + 
             ps_reg_02 + ps_ind_15 + ps_ind_06_bin + ps_car_07_cat + ps_ind_09_bin + 
             ps_reg_01 + ps_car_14 + ps_car_12 + ps_car_11 + ps_car_15 + 
             ps_ind_02_cat + ps_ind_01 + ps_ind_16_bin + ps_car_09_cat + 
             ps_car_02_cat + ps_calc_08 + ps_calc_19_bin + ps_ind_04_cat + 
             ps_car_04_cat + ps_calc_16_bin, family = "binomial", data = train)
#Test it with validation set with gini index
valid.test.step <- predict(glm.step,newdata=valid.sample,type="response")
normalized.gini.index(valid.sample.truth,valid.test.step) 
#  0.2092597

#Predict test set
probs.step <- predict(glm.step, newdata = test, type = "response")
mypreds.step <- cbind(as.character(test$id),probs.step)

#Write out the predictions to a csv file
write.table(mypreds.step, file = "submission_step.csv", row.names=F, col.names=c("id","target"), sep=",")



#####################################
#           Random Forest           #
#####################################

#Use the ranger package
RF1 <- ranger(target~.-id,data=train.sample,write.forest=T)

#Test it with validation set with gini index
valid.test.RF1 <- predict(RF1,data=valid.sample)
normalized.gini.index(valid.sample.truth,valid.test.RF1$predictions) 
#0.9903151

#Predict test set
predsRF1<-predict(RF1,test)
mypredsRF1 <- cbind(as.character(test$id),predsRF1$predictions)

#Write out the predictions to a csv file
write.table(mypredsRF1, file = "submissionRF1.csv", row.names=F, col.names=c("id","target"), sep=",")



#Use the random forest package
# mtry = sqrt(p)
RF2 =randomForest(target~.-id,data=train.sample,mtry=sqrt(54),ntree=500)

#Test it with validation set with gini index
valid.test.RF2 <- predict(RF2,data=valid.sample)
normalized.gini.index(valid.sample.truth,valid.test.RF2) 
# 0.2673451

#Predict test set
predsRF2=predict (RF2, test)
mypredsRF2 <- cbind(as.character(test$id),predsRF2)

#Write out the predictions to a csv file
write.table(mypredsRF1, file = "submissionRF2.csv", row.names=F, col.names=c("id","target"), sep=",")

# mtry = p/2
RF3 =randomForest(target~.-id,data=train.sample,mtry=54/2,ntree=500)

#Test it with validation set with gini index
valid.test.RF3 <- predict(RF3,data=valid.sample)
normalized.gini.index(valid.sample.truth,valid.test.RF3) 
# 0.20123757

#Predict test set
predsRF3=predict (RF3, test)
mypredsRF3 <- cbind(as.character(test$id),predsRF3)

#Write out the predictions to a csv file
write.table(mypredsRF3, file = "submissionRF3.csv", row.names=F, col.names=c("id","target"), sep=",")










#####################################
#                Gini               #
#####################################
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
  optimal.gini.index = unnormalized.gini.index(ground.truth, ground.truth)
  return(model.gini.index / optimal.gini.index)
}
