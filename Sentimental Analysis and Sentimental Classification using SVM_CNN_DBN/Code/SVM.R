# loading neccessary packages and dataset
install.packages("caret")
install.packages("gtable")
install.packages("e1071")
install.packages("readr")
install.packages("matrixStats")
install.packages("ggplot2")
library(ggplot2)
library(gtable)
library(caret)
library(e1071)
library(readr)
library(matrixStats) 
setwd("C:/Users/Nimi/Downloads/r studio/final")
corpus <- read_csv("corpus.csv")
corpus[is.na(corpus)] <- 0
dim(corpus)
#corpus<-corpus[,c(2:7395)]
names(corpus)[8593] <- "Stars"
corpus$Stars <- ifelse(corpus$Stars==1, 'positive', 'negative')

null.check<-data.frame(colSums(is.na(test)))
head(null.check)
names(null.check)[1] <- "nulls"
null.check[null.check$nulls>0,]


corpus[is.na(corpus$cycl),]

index <- sample(1:nrow(corpus),round(0.75*nrow(corpus)))
train <- corpus[index,]
test <- corpus[-index,]
nrow(test)
nrow(train)

#Test time and check ideal parameters
#system.time(svm(Stars ~ ., kernel = "radial", cost = 10,gamma = 0.05,  data = train, scale = F))
tuned <- tune(svm,Stars ~ .,data = train,kernel = "radial",
              type='C-classification',ranges=list(cost=c(0.001,0.01,0.1,1,10,100)),scale=F)
summary(tuned) 

model.r = svm(Stars ~ ., data = train, type='C-classification',scale = F,kernel='radial')
print(model.r)

plot(model.r)

#Based on the input data, ideal costs
# Getting rid of the classifier column
nrow(test[-8593])
ncol(test)
predictions <-  predict(model.r, test[-8593])
predictions <- unlist(predictions)
svm.out<-as.data.frame(predictions)
nrow(svm.out)

table(unlist(test[8593]),unlist(predictions))

a<-confusionMatrix(predictions,unlist(test[8593]))
a

