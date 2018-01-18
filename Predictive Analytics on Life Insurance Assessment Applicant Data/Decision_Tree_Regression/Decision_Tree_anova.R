#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# install.packages("forecast")
# library(forecast)

setwd("F:/INFO7390-ADS/Mid-term_Project/train.csv")

training_data <- read.csv(file = "Final_Training_Data.csv")
test_data <-read.csv(file = "Final_Test_Data.csv")
tree <- rpart(response~., training_data, method = "anova")

prp(tree)

plotcp(tree)

rsq.rpart(tree)

prune_tree<- prune(tree, cp=0.011)

prp(prune_tree)

predict_data <- round(predict(prune_tree, test_data, type = "vector"))

test_data$response_predicted <- predict_data


#Writing predicted and actual response to csv file
write.csv(test_data, file = "F:/INFO7390-ADS/Mid-term_Project/Decision_Tree_Regression_Predicted_Test_Data_Anova.csv")


data <- read.csv(file = "F:/INFO7390-ADS/Mid-term_Project/Decision_Tree_Regression_Predicted_Test_Data_Anova.csv")

#Root Mean Square Error
RMSE <- sqrt(mean((test_data[,53]- predict_data)^2))
RMSE #2.65

# this will give u accuracy #8.47%
count = 0
for(i in 1:nrow(test_data)){
  if(test_data[i,53] == test_data[i,54]){
    count = count + 1
  }
}
count/9382*100

#accuracy(test_data$response_predicted, test_data$response)
