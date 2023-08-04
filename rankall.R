rankall <- function(outcome, num = "best"){
  data <- read.csv('outcome-of-care-measures.csv')
  outcomes <- c('11'='heart attack',
                '17'='heart failure', '23'='pneumonia')
  
  if(!(outcome %in% outcomes)){
    stop('invalid outcome')
  }
  
  finaldata <- data.frame(matrix(ncol=3))
  col <- c('hospital', 'state', 'number1')
  colnames(finaldata) <- col
  len=1
  
  for(state in unique(data[,7])){
    if(num=='worst'){
      statedata <- data[data[,7]==state, c(2, 7, 
                                           as.numeric(names(outcomes)[outcomes==outcome]))]
      
      good <- statedata[,3]=='Not Available'
      statedata <- statedata[!good,1:3]
      worstnum <- nrow(statedata)
      
      statedata[,3] <- as.numeric(statedata[,3])
      
      ranking <- order(statedata[,3], statedata[,1])
      ordereddata <- statedata[ranking, 1:3]
      
      hospitalname <- ordereddata[worstnum, 1]
      
      finaldata[len,2] <- state
      finaldata[len,1] <- hospitalname
      finaldata[len,3] <- nrow(statedata)
      
      len <- len + 1
    } else if(num=='best'){
      num <- 1
      statedata <- data[data[,7]==state, c(2, 7, 
                                           as.numeric(names(outcomes)[outcomes==outcome]))]
      
      good <- statedata[,3]=='Not Available'
      statedata <- statedata[!good,1:3]
      worstnum <- nrow(statedata)
      
      statedata[,3] <- as.numeric(statedata[,3])
      
      ranking <- order(statedata[,3], statedata[,1])
      ordereddata <- statedata[ranking, 1:3]
      
      hospitalname <- ordereddata[num, 1]
      
      finaldata[len,2] <- state
      finaldata[len,1] <- hospitalname
      finaldata[len,3] <- nrow(statedata)
      
      len <- len + 1
    } else {
      statedata <- data[data[,7]==state, c(2, 7, 
                                           as.numeric(names(outcomes)[outcomes==outcome]))]
      
      good <- statedata[,3]=='Not Available'
      statedata <- statedata[!good,1:3]
      worstnum <- nrow(statedata)
      
      statedata[,3] <- as.numeric(statedata[,3])
      
      ranking <- order(statedata[,3], statedata[,1])
      ordereddata <- statedata[ranking, 1:3]
      
      hospitalname <- ordereddata[num, 1]
      
      finaldata[len,2] <- state
      finaldata[len,1] <- hospitalname
      finaldata[len,3] <- nrow(statedata)
      
      len <- len + 1
    }
    
  }
  
  ranking2 <- order(finaldata[,2])
  orderedfinaldata <- finaldata[ranking2, 1:2]

  orderedfinaldata

  
}