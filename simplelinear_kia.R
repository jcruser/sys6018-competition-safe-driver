library(readr)


#Read in data
setwd("~/Desktop/SYS 6018/Kaggle competitions/safe driver")
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

lm <- lm(target~.-id,data=train)
sig_vars <- summary(lm)$coeff[-1,4] < 0.05
sig_vars <- names(sig_vars)[sig_vars == TRUE] 
sig_formula <- as.formula(paste("target ~",sig_vars)) 

lm_sig <- lm(formula=sig_formula,data=train)
probs <- predict(lm_sig, newdata = test, type = "response")
preds <- rep(0,892816)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
mypreds <- cbind(as.numeric(sample$id),preds)
mypreds

#Write out the predictions to a csv file
write.table(mypreds, file = "submission1.csv", row.names=F, col.names=c("id","target"), sep=",")

