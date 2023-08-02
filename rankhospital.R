rankhospital <- function(state, outcome, num){
  data <- read.csv('outcome-of-care-measures.csv')
  outcomes <- c('11'='heart attack', '17'='heart failure', 
                '23'='pneumonia')

  if(!(state %in% data[,7])){
    stop('invalid state')
  }
  if(!(outcome %in% outcomes)){
    stop('invalid outcome')
  }
  
  statedata <- data[data[,7]==state, c(2, 7, 
                as.numeric(names(outcomes)[outcomes==outcome]))]
  
  statedata[,3] <- as.numeric(statedata[,3])
  
  ranking <- order(statedata[,3], statedata[,1])
  ordereddata <- statedata[ranking, 1:3]
  
  good2 <- complete.cases(ordereddata[,3])
  ordereddata <- ordereddata[good2,1:3]
  
  if(num=='best'){num <- 1}
  if(num=='worst'){num <- nrow(ordereddata)}
  
  hospitalname <- ordereddata[num, 1]
  
  hospitalname
}