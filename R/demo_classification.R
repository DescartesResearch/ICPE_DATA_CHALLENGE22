library(randomForest)
library(caret)

source("feature_calculation.R")

# read trained classification model
model <- readRDS("classification_model.RDS")

# read demo data
data <- read.csv("demo.csv", sep=";")

# get all not triaged commits
ind <- which(data[,2]=="not_triaged" | data[,2]=="under_investigation")

# get all change points
cps <- which(data[,2]=="true_positive")

# get measurement data of commits
values <- data[,1]

classification <- c()

for(p in 1:length(ind)){
  # first commit cannot be a change point
  if(p==1){
    classification <- c(classification, FALSE)
    next
  } 
  
  # get last change point
  P_minus <- cps[sort(which(cps < p),T)[1]]
  # if there is no change point, set to first point
  if(is.na(P_minus)){
    P_minus <- 1
  }
  
  # get next change point
  p_plus <- cps[which(cps>p)[1]]
  # if there is no change point, set to length of time series
  if(is.na(p_plus)){
    p_plus <- nrow(data)
  }
  
  pre <- values[P_minus:(p-1)]
  after <- values[i:p_plus]
  
  # calculate features on pre and after
  features <- calculation(pre, after)
  
  # get proabilities for this commit - just for information
  probs <- predict.train(model,t(features), type="prob")
  
  # get prediction for this commit
  pred <- predict.train(model,t(features))
  classification <- c(classification, pred)
  
}

