# SVM Model
#Importing caret and e1071 libraries

library(caret)
library(e1071)

setwd("F:/INFO7390-ADS/Mid-term_Project/train.csv")

#reading the training dataset from the csv
prudentialData_Train <- read.csv(file="Final_Training_Data.csv", header=TRUE, sep= ',')

# reading the test dataset from the csv
prudentialData_Test <- read.csv(file="Final_Test_Data.csv", header=TRUE, sep= ',')

#identify the structure of the train dataset
str(prudentialData_Train)

#identify the structure of the test dataset
dim(prudentialData_Train)


#Tuning the svm models. 
#We will get optimal cost and gamma parameter which we will use in next section for creating svm model.
sample_index = sample(49999, 1000)
train <- prudentialData_Train[sample_index,]

#tuning the dataset using "radial kernel"
svm_tune_rad <- tune(svm, response ~ .,
                      data = train,
                      kernel="radial",
                      ranges=list(cost=10^(-1:2),
                                  gamma=c(.5,1,2),
                      scale=F
                      ))
#summary of the tuning
sumry_rad <- summary(svm_tune_rad)
sumry_rad


#Cost is general penalizing parameter, it's a cost of penalizaling for misclassification. So if C is large the bias will be low and variance high.
#Gamma is parameter of Guassian kernel, used for nonlinear structures. In our same, for linear optimal cost is 1, for radial it is 10 and gamma 0.5. We will use these values for build our svm model.
svm_fit_radial <- svm(prudentialData_Train$response ~ ., kernel="radial", cost = 10, gamma=0.5,data = prudentialData_Train,scale=F)
print(svm_fit_radial)


#Now we will predict the Credit Worthiness (response) feature for all the svm models with radial kernel and later in next section we will check for the accuracy of the prediction.

predictions <-  predict(svm_fit_radial, prudentialData_Test[-55])

#Root Mean Square Error
RMSE <- sqrt(mean((prudentialData_Test[,55]-predictions)^2))
RMSE #2.35

SVM_Regression_Predicted_Actual_Test_Data<-cbind(prudentialData_Test[,c(1,55)],predictions)

#Writing predicted and actual response to csv file
write.csv(SVM_Regression_Predicted_Actual_Test_Data, file = "F:/INFO7390-ADS/Mid-term_Project/SVM_Regression/SVM_Regression_Predicted_Test_Data.csv")

