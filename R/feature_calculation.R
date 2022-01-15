library(tsfeatures)

#' @description Handels the feature calculation
#'
#' @title Calculates the feature of pre and after
#' @param pre Vector of measurements before the commit
#' @param after Vector of measurements starting with the commit
#' @return The combined features of pre and after
calculation <- function(pre, after){
  
  # normalize pre and after
  tmp <- c(pre,after)
  tmp <- normalize(tmp)
  
  m.pre <- mean(tmp[1:length(pre)])
  sd.pre <- sd(tmp[1:length(pre)])
  
  # if pre or after is constant set sd to 0
  if(is.na(sd.pre)) sd.pre <- 0
  m.after <- mean(tmp[-(1:length(pre))])
  sd.after <- sd(tmp[-(1:length(pre))])
  if(is.na(sd.after)) sd.after <- 0
  
  # caclulate features
  feat.pre <- calcF(pre)
  feat.after <- calcF(after)
  
  res <- c(m.pre, sd.pre, feat.pre,
           m.after, sd.after, feat.after
  )
  
  names(res) <- c("pre_norm_mean", "pre_norm_sd", paste("pre", names(feat.pre), sep="_"), 
                  "after_norm_mean", "after_norm_sd", paste("after", names(feat.after), sep="_")
  )
  
  return(res)
  
}

#' @description Calculates the features
#'
#' @title Calculates the feature
#' @param x Vector of measurements 
#' @return Features of x
calcF <- function(x){
  
  N <- length(x)
  if(N <= 2){
    # some features require a mininmal length
    x <- rep(x, length.out = 4)
  }
  
  # the number of lags
  l <- ifelse(N>10,10,2)
  
  res <- c(
    lumpiness(x, width = l),
    stability(x, width = l),
    checkNa(spectralentropy(x),1),
    crossing_points(x),
    checkNa(max_level_shift(x, width = l)[1],0),
    hurst(x),
    addName(checkNa(Box.test(x,lag=l)$statistic/(length(x)*l),1), "serial_correlation")
  )
  
  return(res)
  
  
}

#' @description Check for NA in calculation and if so replaces with default value
#'
#' @title Check and replace NAs
#' @param x Value
#' @param d Default value
#' @return A valid value
checkNa <- function(x,d){
  m <- names(x)
  if(is.na(x)){
    x <- d
    names(x) <- m
  }
  return(x)
}

#' @description Names a value
#'
#' @title Naming a value
#' @param x Value
#' @param s Name
#' @return Named value
addName <- function(x,s){
  names(x) <- s
  return(x)
}

#' @description Normalized a vector according to min-max-scaling
#'
#' @title Min-Max-Normalization
#' @param x Vector
#' @return Normalized vector
normalize <- function(x, na.rm = TRUE) {
  if(length(table(x))==1) return(x)
  return((x- min(x)) /(max(x)-min(x)))
}

#' @description Calculates the spectral entropy of vector
#'
#' @title Spectral Entropy
#' @param x Vector of measurements
#' @return Spectral entropy of input
spectralentropy <- function(x) {
  entropy <- tryCatch({
    spec <- stats::spec.ar(na.contiguous(x), plot=FALSE, method='burg', 
                             n.freq = ceiling(length(x)/2 + 1))
    fx <- c(rev(spec$spec[-1]),spec$spec)/ length(x)
    fx <- fx/sum(fx)
    prior.fx = rep(1 / length(fx), length = length(fx))
    prior.weight = 0.001
    fx <- (1 - prior.weight) * fx + prior.weight * prior.fx
    pmin(1, -sum(fx * log(fx, base = length(x))))
  }, error=function(e){
    NA
  })
  return(c(entropy = entropy))
}