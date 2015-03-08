best <- function(inState, condition) {
  
  ## Read outcome data
  outcome <-read.csv("outcome-of-care-measures.csv", colClasses="character")
  names(outcome)[11] <- "heart.attack"
  outcome$heart.attack <- suppressWarnings(as.numeric (outcome$heart.attack))
  names(outcome)[17] <- "heart.failure"
  outcome$heart.failure <- suppressWarnings(as.numeric (outcome$heart.failure))
  names(outcome)[23] <- "pneumonia"
  outcome$pneumonia <- suppressWarnings(as.numeric (outcome$pneumonia))
    
  ## Check that state and outcome are valid
  validConditions = c('heart attack','heart failure','pneumonia')
  if (!condition %in% validConditions) {
    stop ("invalid outcome")  
  }
  
  validStates = unique (outcome$State)
  if (!inState %in% validStates) {
    stop ("invalid state")  
  }
  
  hospitals <- subset(outcome, State == inState)
  
  if (condition == "heart attack") {
    sorted <- hospitals[order(hospitals$heart.attack, hospitals$Hospital.Name),]
  }
  else if (condition == "heart failure") {
    sorted <- hospitals[order(hospitals$heart.attack, hospitals$Hospital.Name),]
  }
  else {
    sorted <- hospitals[order(hospitals$heart.attack, hospitals$Hospital.Name),]
  }
  
  return (sorted[1,]$Hospital.Name)
}