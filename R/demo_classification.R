library(randomForest)
library(caret)

source("feature_calculation.R")

model <- readRDS("classification_model.RDS")

data <- read.csv("demo.csv", sep=";")

ind <- which(data$MongoDB.Label=="not_triaged" | data$MongoDB.Label=="under_investigation")
cps <- which(data$MongoDB.Label=="true_positive")

classification <- c()

for(p in 1:length(ind)){
  if(p==1){
    classification <- c(classification, FALSE)
    next
  } 
  
  P_minus <- cps[sort(which(cps < p),T)[1]]
  if(is.na(P_minus)){
    P_minus <- 1
  }
  pre <- data[P_minus:(p-1),1]
  p_plus <- cps[which(cps>p)[1]]
  if(is.na(p_plus)){
    p_plus <- nrow(data)
  }
  after <- data[i:p_plus,1]
  features <- calculation(pre, after)
  probs <- predict.train(model,t(features), type="prob")
  pred <- predict.train(model,t(features))
  classification <- c(classification, pred)
  
}

