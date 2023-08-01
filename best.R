best <- function(state, outcome){
  medical <- read.csv("outcome-of-care-measures.csv")
  outcomes <- c('11'="heart attack",'17'="heart failure",
                '23'="pneumonia")                        #read data and setting outcome index
  
  if(!(state %in% medical[, 7])){
    stop("invalid state")
  }                                                      ##check if argument is valid
  
  if(!(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  
  statecorrect <- medical[,7] == state                        ##make data with input state
  statecorrectdatas <- medical[statecorrect,
    c(2, 7, as.numeric(names(outcomes)[outcomes==outcome]))]
  
  statedatasbad <- is.na(statecorrectdatas[,3])               ##removing NAs
  finaldata <- statecorrectdatas[!statedatasbad, 1:3]
  
  returnvalue <- finaldata[which.min(finaldata[,3]), 1]       ##find hospital name having minimum value
  
  returnvalue
}



