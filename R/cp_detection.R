#' @description Detects change points in previous and current data
#'
#' @title Detects change points
#' @param previous Vector of previous measurements
#' @param commit Current measurement
#' @param cps Optional parameter: The found change points in this time series, default = NULL 
#' @param model The classification model
#' @return The classification of the commit
cp_detection <- function(previous, commit, cps=NULL, model){
  
  
  if(is.null(cps)){
    # set start to 1 if there are not any change points yet in the time series
    s <- 1
  } else {
    # otherwise get last found change point
    s <- tail(cps,1)
  }
  
  pre <- previous[s:length(previous)]
  after <- commit
  
  # calculate features on pre and after
  features <- calculation(pre, after)
  
  # get proabilities for this commit - just for information
  probs <- predict.train(model,t(features), type="prob")
  
  # get prediction for this commit
  pred <- ifelse(predict.train(model,t(features))==T,T,F)
  
  return(pred)
}

