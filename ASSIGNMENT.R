

setwd("C:\\Users\\Downloads\\Data")
# set up my working directory 

id_train=read.csv("data_train.csv",stringsAsFactors = T)
# importing my data in by using read csv funtion

id_test=read.csv("data_test.csv",stringsAsFactors = T)



id_test$target=NA
#making null values in test dataset

id_test$data='test'
id_train$data='train'

id_all=rbind(id_train,id_test)

# combining both training and testing data set

glimpse(id_all)


lapply(id_all,function(x) sum(is.na(x)))

# to figure out the missing values

id_all$num18[is.na(id_all$num18)] <- mean(id_all$num18, na.rm=TRUE)
id_all$num19[is.na(id_all$num19)] <- mean(id_all$num19, na.rm=TRUE)
id_all$num20[is.na(id_all$num20)] <- mean(id_all$num20, na.rm=TRUE)
id_all$num21[is.na(id_all$num21)] <- mean(id_all$num21, na.rm=TRUE)
id_all$num22[is.na(id_all$num22)] <- mean(id_all$num22, na.rm=TRUE)
id_all$num18[is.na(id_all$num18)] <- mean(id_all$num18, na.rm=TRUE)
id_all$cat1[is.na(id_all$cat1)] <- mean(id_all$cat1, na.rm=TRUE)
id_all$cat2[is.na(id_all$cat2)] <- mean(id_all$cat2, na.rm=TRUE)
id_all$cat3[is.na(id_all$cat3)] <- mean(id_all$cat3, na.rm=TRUE)
id_all$cat4[is.na(id_all$cat4)] <- mean(id_all$cat4, na.rm=TRUE)
id_all$cat5[is.na(id_all$cat5)] <- mean(id_all$cat5, na.rm=TRUE)
id_all$cat6[is.na(id_all$cat6)] <- mean(id_all$cat6, na.rm=TRUE)
id_all$cat8[is.na(id_all$cat8)] <- mean(id_all$cat8, na.rm=TRUE)
id_all$cat10[is.na(id_all$cat10)] <- mean(id_all$cat10, na.rm=TRUE)
id_all$cat12[is.na(id_all$cat12)] <- mean(id_all$cat12, na.rm=TRUE)


#replacing missing values by mean of that particular column

library(dplyr)

# removing the varialble train and test from main data set
id_train=id_all %>% filter(data=='train') %>% select(-data)
id_test=id_all %>% filter(data=='test') %>% select(-data,-target)


# Dividing the dataset into training and testing to bulid model on training data and checking accuracy on test data
set.seed(2)
s=sample(1:nrow(id_train),0.7*nrow(id_train)) 
valtrain=id_train[s,]
valtest=id_train[-s,]

#using glm fuction on traning data

fit=glm(target~.,data = valtrain,family = 'binomial')

library(car)

# to check the significant variables for target variable to make predictions


fit=step(fit)
summary(fit)

# this are my significant variables

fit=glm(target~num1+num2+num4+num5+num12+num13+num14+num15+num16+num17+num18+num19+num20+num21+num22+num23+der3+ der6+der14+cat1+cat2 +cat3+cat5+cat6+cat8+cat9+cat10+cat11+cat12 ,data = valtrain,family = 'binomial')

summary(fit)

# predicting on tesing data to check acccuracy with cutoff as 0.4

val.pred=predict(fit,newdata = valtest,type = 'response')

p=table(Actualvalue=valtest$target,predictedValue=val.pred>0.4)
p


(172228+1)/(6517+172282+0+1)

# accuracy of 95 %


formula(fit)

library(pROC)

test_roc <- roc(valtest$target ~ val.pred, plot = TRUE, print.auc = T)


#model for prediction on entire data

fit.final=glm(target~.,data = id_train,family = 'binomial')

fit.final=step(fit.final)

  fit.final=glm(target~num1+num2+num4+num5+num12+num13+num14+num15+num16+num17+num18+num19+num20+num21+num22+num23+der3+ der6+der14+cat1+cat2 +cat3+cat5+cat6+cat8+cat9+cat10+cat11+cat12 ,data = id_train,family = 'binomial')

summary(fit.final)


test.pred=predict(fit.final,newdata = id_test,type = 'response')

write.csv(test.pred,'submissoin_logistic.csv',row.names = F)

