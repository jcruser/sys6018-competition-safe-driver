library(readr)
library(boot)
library(dplyr)

#Read in data

train <- read.csv("train.csv")
test <- read.csv("test.csv")

### Data Cleaning and Exploration #####

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


##### basic lm #####
#not the right type of model but starts to give insights on some of the vars
basic.lm <- lm(target~.-id,data=train)
summary(basic.lm)

#vars with potential
#ps_ind_03
#ps_ind_04_cat
#ps_ind_05_cat
#ps_ind_06_bin
#ps_ind_07_bin
#ps_ind_08_bin 
#ps_ind_15 
#ps_ind_16_bin  
#ps_ind_17_bin  
#ps_ind_18_bin  
#ps_reg_02 
#ps_car_03_cat
#ps_car_07_cat 
#ps_car_11      
#ps_car_12        
#ps_car_13
#ps_calc_19_bin

#######  model exploring sig values #########
#still an lm so not the right model
sigvals.lm <- lm(target~ ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_06_bin + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin+ 
                   ps_ind_17_bin + ps_ind_18_bin + ps_reg_02 + ps_car_03_cat + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13+ps_calc_19_bin, data=train )

summary(sigvals.lm)
#ps_ind_06_bin and ps_calc_19_bin  no longer significant

#look at resids
sigvalsresid <-resid(sigvals.lm)
qqnorm(sigvalsresid)
qqline(sigvalsresid)
## definitely some issues here

yhat <- fitted(sigvals.lm)
plot(yhat,sigvalsresid)
##have two parallel lines

### model without ps_ind_06_bin and ps_calc_19_bin

sigvals2.lm <- lm(target~ ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin+ 
                   ps_ind_17_bin + ps_ind_18_bin + ps_reg_02 + ps_car_03_cat + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13, data=train )

summary(sigvals2.lm)

#look at resids
sigvalsresid2 <-resid(sigvals2.lm)
qqnorm(sigvalsresid2)
qqline(sigvalsresid2)
## definitely some issues here, part is a linear and then jumps up

yhat2 <- fitted(sigvals2.lm)
plot(yhat2,sigvalsresid2)
##have two parallel lines



############## Log Regression #######################
## using the same sign vals as the previous model
basic.glm = glm(target~ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin+ 
                  ps_ind_17_bin + ps_ind_18_bin + ps_reg_02 + ps_car_03_cat + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13, data = train, family='binomial')

summary(basic.glm)

## apply model to test data
probs_log = predict(basic.glm, test, type = "response")
probs_log[1:10]
target <- ifelse(probs_log > 0.5, 1, 0)
target [1:10]

#target <- rep(0,892816)  # Initialize prediction vector
#target[probs_log>0.5] <- 1 

### apply model to test data

log_preds <- cbind(as.integer(test$id),target)
log_preds[1:10]

colnames(log_preds)[1] <- "id"


#write to csv
write.table(log_preds, file = "c:/Users/Jennifer/Desktop/SafeDriveLog.csv", row.names=F, col.names=T, sep=",")

######## Log Reg with kfold ############

library(caret)

# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
kfold_logr<- train(target~ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_07_bin + ps_ind_15 + ps_ind_16_bin+ ps_ind_17_bin + ps_ind_18_bin + 
                     ps_reg_02 + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13, data=train.clean, trControl=train_control, 
                   method="glm", family=binomial())

# print cv scores
summary(kfold_logr)

#look at results
kfold_logr$results

## new log reg with more limited var

kfold.glm = glm(target~ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_07_bin + ps_ind_15 + ps_ind_16_bin+ ps_ind_17_bin + ps_ind_18_bin + 
                  ps_reg_02 + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13, data = train.clean, family='binomial')


## apply model to test data
probs <- predict(kfold.glm, newdata = test, type = "response")
mypreds <- cbind(as.character(test$id),probs)


#write to csv
write.table(mypreds, file = "c:/Users/Jennifer/Desktop/SafeDriveLogk.csv", row.names=F, col.names=T, sep=",")


###### apply gini index function ######
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



  