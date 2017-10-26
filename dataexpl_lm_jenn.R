library(readr)


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

sigvals.lm <- lm(target~ ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat + ps_ind_06_bin + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin+ 
                   ps_ind_17_bin + ps_ind_18_bin + ps_reg_02 + ps_car_03_cat + ps_car_07_cat + ps_car_11 + ps_car_12+ps_car_13+ps_calc_19_bin, data=train )

summary(sigvals.lm)





  