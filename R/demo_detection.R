library(randomForest)
library(caret)

source("feature_calculation.R")
source("cp_detection.R")

# read trained classification model
model <- readRDS("classification_model.RDS")

# read demo data
data <- read.csv("demo.csv", sep=";")

# get measurement data of commits
values <- data[,1]

detection <- c()

cps <- c()

# emulate new incoming commits
if(length(values)>1){
  for(p in 2:length(values)){
    
    previous <- values[1:(p-1)]
    commit <- values[p]
    pred <- cp_detection(previous = previous, commit = commit, cps = cps, model = model)
    if(pred){
      cps <- c(cps,p)
    }
    
  }
}