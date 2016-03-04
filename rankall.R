rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  #Valid outcome check
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid.outcomes) {
    #return(sprintf("Error in rankhospital(%s, %s, %s) : invalid state",state,outcome,num))
    stop("invalid outcome")
  }
  
  #Read dataset .
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Coerce dependent on outcome/suppres warnings
  if (outcome == "heart attack") {index<-11}
  else if (outcome == "heart failure") {index<-17}
  else {index<-23
  }
  
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  
  #remove na
  data <- na.omit(data)
  
  split.state.data <- split(data, data$State)
  
  result = lapply(split.state.data, function(x,num){
    x=x[order(x[index], x$Hospital.Name),]
    #check num
    if (num == "best") {
      return (x$Hospital.Name[1])
    } else if (num == "worst") {
      return (x$Hospital.Name[nrow(x)])
    } else if (is.numeric(num)) {
      return (x$Hospital.Name[num])
    } 
    else {
      #return(sprintf("Error in rankhospital(%s, %s, %s) : invalid outcome",state,outcome,num))
      stop("invalid num")
    }    
  },num)

  return(data.frame(hospital=unlist(result),state=names(result)))
}

  ##Test
  ##debug("rankall")
  ## 


  

  
  
  #Return hospital name in that state with the given rank
  #state.data[num]
