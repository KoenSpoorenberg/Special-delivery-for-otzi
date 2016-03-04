best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #Valid outcome check
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid.outcomes) {
    #return(sprintf("Error in best(%s, %s) : invalid state",state,outcome))
    stop("invalid outcome")
  }
  
  #Read dataset .
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Valid state check
  states <- table(data$State)
  if (!state %in% names(states)) { 
    #return(sprintf("Error in best(%s, %s) : invalid outcome",state,outcome))
    stop("invalid state")
  }  
  
  #Coerce dependent on outcome/suppres warnings
  if (outcome == "heart attack") {index<-11}
  else if (outcome == "heart failure") {index<-17}
  else {index<-23
  }
  
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  #remove na
  data <- na.omit(data)
  
  #Subset State data
  state.data <- subset(data, State==state)
  # 
  state.data <- state.data[order(state.data[,index], na.last=TRUE),2]
  state.data <- na.omit( state.data)
  
  #Get hospital name with the lowest 30-day mortality rate.
  state.data[1]
}

## test script
## best("TX", "heart attack")
## best("TX", "heart failure")
## best("MD", "heart attack")
## best("MD", "pneumonia")
## best("BB", "heart attack")
## best("NY", "hert attack")