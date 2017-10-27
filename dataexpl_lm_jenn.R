library(readr)
library(boot)
library(dplyr)

#Read in data

train <- read.csv("train.csv")
test <- read.csv("test.csv")
sample <- read.csv("sample_submission.csv")

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
train[train==-1] <- NA
test[test==-1] <- NA

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

basic.glm = glm(target~ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin+ 
                  ps_ind_17_bin + ps_ind_18_bin + ps_reg_02 + ps_car_03_cat + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13, data = train, family='binomial')

summary(basic.glm)

## apply model to test data
preds_log = predict(basic.glm, newdata = test, se=TRUE)
preds <- rep(0,892816)  # Initialize prediction vector


### apply model to test data
probs <- predict(sigvals2.lm, newdata = test, type = "response")
preds <- rep(0,892816)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
mypreds <- cbind(as.numeric(sample$id),preds)
mypreds

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



  