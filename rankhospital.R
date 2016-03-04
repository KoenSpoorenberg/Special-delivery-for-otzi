
setwd("C:\\Users\\Koen\\Documents\\Assignment 3\\" )
getwd()

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  #Valid outcome check
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid.outcomes) {
    #return(sprintf("Error in rankhospital(%s, %s, %s) : invalid state",state,outcome,num))
    stop("invalid outcome")
  }

  #Read dataset .
  data <- read.csv("outcome-of-care-measures.csv")
  
  #Valid state check
  states <- table(data$State)
  if (!state %in% names(states)) { 
    #return(sprintf("Error in rankhospital(%s, %s, %s) : invalid outcome",state,outcome,num))
    stop("invalid state")
  }  
  
  #Coerce dependent on outcome/suppres warnings
  if (outcome == "heart attack") {index<-11}
  else if (outcome == "heart failure") {index<-17}
  else {index<-23
  }
  
  

  data[,index] <- suppressWarnings(as.numeric(data[,index]))

  #remove na
  #data <- na.omit(data)
  
  #Subset State data
  state.data <- subset(data, State==state)
  
  # 
  state.data <- state.data[order(state.data[,index], state.data[,2], na.last=NA),2]
  state.data <- na.omit(state.data)
  
  #check num
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num=length(state.data)
  } else if (is.numeric(num)) {
    num=num
  } 
  else {
    #return(sprintf("Error in rankhospital(%s, %s, %s) : invalid outcome",state,outcome,num))
    stop("invalid num")
  }
  
  
  #Return hospital name in that state with the given rank
  state.data[num]
}

## tests:
## debug("rankhospital")


## source("rankhospital.R")
## rankhospital("TX", "heart failure", 4)
## "DETAR HOSPITAL NAVARRO"
## rankhospital("MD", "heart attack", "worst")
## "HARFORD MEMORIAL HOSPITAL"
## rankhospital("MN", "heart attack", 5000)
##  NA

