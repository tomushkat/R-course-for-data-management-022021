


continuousOutliers <- function(data, lowerBound = 3, upperBound = 3){
  
  # This function takes a vector, and replace observations that are smaller or bigger than the
  # lower/upper bounds * standard deviations with NA.
  
  Mean   <- mean(data, na.rm = TRUE)
  SD     <- sd(data, na.rm = TRUE)
  High   <- Mean + upperBound * SD # Upper Range  
  Low    <- Mean - lowerBound * SD # Lower Range
  Final  <- ifelse(data > High | data < Low, NA, data)
  
  return(Final)
  
}

intervalOutliers <- function(data, lowerBound = 2.5, upperBound = 2.5){
  
  # This function takes a vector, and replace observations that are smaller or bigger than the
  # lower/upper bounds * interquartile range with NA.
  
  Q      <- quantile(data, probs = c(.25, .75), na.rm = TRUE)
  IQR    <- IQR(data, na.rm = TRUE)
  High   <- Q[2] + upperBound * IQR # Upper Range
  Low    <- Q[1] - lowerBound * IQR # Lower Range
  Final  <- ifelse(data > High | data < Low, NA, data)
  return(Final)
}


removeSingles <- function(data, varString, N){
  
  # This function removes rows from the data that their target variable does not repeat N times,
  # and organize the data set by 'varString'.
  # The function get the (1) data set, (2) the name of the variables as a string/character,
  # and (3) the number of correct repetitions.
  
  require(tidyverse)
  
  dataSet                       <- as.data.frame(table(data[, varString]))
  dataSet                       <- dataSet %>% mutate(Logical = ifelse(Freq == N, 'OK', NA))
  dimnames(dataSet)[[2]]        <- c(varString, 'Freq', 'Logical')
  data1                         <- merge(data, dataSet, by = varString, all = TRUE)
  col                           <- which(colnames(data1) == varString)
  data2                         <- data1[!is.na(col), ]
  data2[, c('Freq', 'Logical')] <- NULL
  
  return(data2)
}



