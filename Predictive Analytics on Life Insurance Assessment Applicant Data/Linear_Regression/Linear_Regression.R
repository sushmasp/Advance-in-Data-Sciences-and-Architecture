setwd("F:/INFO7390-ADS/Mid-term_Project/train.csv")

Training_Data <- read.csv(file="Final_Training_Data.csv", header=TRUE, sep= ',')

Test_Data <- read.csv(file="Final_Test_Data.csv", header=TRUE, sep= ',')

linear_regression_model = lm(Training_Data$response~., data = Training_Data) #Multiple R squared value: 33.31
summary(linear_regression_model)

# Run predictions the built model
Linear_Regression_Predicted_Response <-  predict(linear_regression_model, Test_Data[-53]) 
#Using the model developed,53rd column i.e. "response" of the Test_Data is being predicted.So the data given here is Test_Data except 53rd column

#id, Actual response, Predicted response
Linear_Regression_Predicted_Actual_Test_Data<-cbind(Test_Data[,c(1,53)],Linear_Regression_Predicted_Response)

#Writing predicted and actual response to csv file
write.csv(Linear_Regression_Predicted_Actual_Test_Data, file = "F:/INFO7390-ADS/Mid-term_Project/Linear_Regression/Linear_Regression_Predicted_Test_Data.csv")


#Root Mean Square Error
RMSE <- sqrt(mean((Test_Data[,53]-Linear_Regression_Predicted_Response)^2))
RMSE

#Linear Regression Plot
plot(linear_regression_model)





