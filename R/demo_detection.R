library(randomForest)
library(caret)

source("feature_calculation.R")

model <- readRDS("classification_model.RDS")

data <- read.csv("demo.csv", sep=";")

values <- data[,1]

detection <- c()

start <- 1
if(length(values)>1){
  for(p in 2:length(values)){
    pre <- values[start:(p-1)]
    after <- values[p]
    features <- calculation(pre,after)
    probs <- predict.train(model,t(features), type="prob")
    pred <- ifelse(predict.train(model,t(features))==T,T,F)
    if(pred){
      start <- p
      detection <- c(detection, p)
    }
  }
}