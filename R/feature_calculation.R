library(tsfeatures)

calculation <- function(pre, after){
  
  tmp <- c(pre,after)
  tmp <- normalize(tmp)
  
  m.pre <- mean(tmp[1:length(pre)])
  sd.pre <- sd(tmp[1:length(pre)])
  if(is.na(sd.pre)) sd.pre <- 0
  m.after <- mean(tmp[-(1:length(pre))])
  sd.after <- sd(tmp[-(1:length(pre))])
  if(is.na(sd.after)) sd.after <- 0
  
  pre.r <- calcF(pre)
  after.r <- calcF(after)
  
  res <- c(m.pre, sd.pre, pre.r,
           m.after, sd.after, after.r
  )
  
  names(res) <- c("pre_norm_mean", "pre_norm_sd", paste("pre", names(pre.r), sep="_"), 
                  "after_norm_mean", "after_norm_sd", paste("after", names(after.r), sep="_")
  )
  
  return(res)
  
}

calcF <- function(x){
  
  N <- length(x)
  if(N <= 2){
    #x <- jitter(rep(x, length.out = 3))
    x <- rep(x, length.out = 4)
  }
  
  l <- ifelse(N>10,10,2)
  
  res <- c(
    lumpiness(x, width = l),
    stability(x, width = l),
    checkNa(entropy(x),1),
    crossing_points(x),
    checkNa(max_level_shift(x, width = l)[1],0),
    hurst(x),
    addName(checkNa(Box.test(x,lag=l)$statistic/(length(x)*l),1), "serial_correlation")
  )
  
  return(res)
  
  
}

checkNa <- function(x,d){
  m <- names(x)
  if(is.na(x)){
    x <- d
    names(x) <- m
  }
  return(x)
}

addName <- function(x,s){
  names(x) <- s
  return(x)
}

normalize <- function(x, na.rm = TRUE) {
  if(length(table(x))==1) return(x)
  return((x- min(x)) /(max(x)-min(x)))
}