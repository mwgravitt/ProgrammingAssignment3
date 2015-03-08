
subsetandsort <- function (inHospitals, inState, columnName) {
  
  # subset to just a single state and remove NA
  inHospitals <- subset (inHospitals, !is.na(inHospitals[[columnName]]) & inHospitals$State == inState)
  
  # sort by the appropriate column, then by hospital name
  sorted <- inHospitals[order(inHospitals[[columnName]], inHospitals$Hospital.Name),]
  return (sorted)
}

rankhospital <- function(inState, condition, inRank = "best") {
  
  ## Read outcome data
  outcome <-read.csv("outcome-of-care-measures.csv") #, colClasses="character")
  
  validCondition <- c("heart attack", "heart failure", "pneumonia")
  columnNum <- c(11,17,23)
  conditionsMetadata = data.frame(validCondition, columnNum)
  
  outcome[columnNums[1]]
  
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
  
  if (condition == "heart attack") {
    sorted <- subsetandsort(outcome, inState, "heart.attack")
  }
  else if (condition == "heart failure") {
    hospitals <- subset (hospitals, !is.na(hospitals$heart.failure))
    sorted <- hospitals[order(hospitals$heart.failure, hospitals$Hospital.Name),]
  }
  else {
    hospitals <- subset (hospitals, !is.na(hospitals$pneumonia))
    sorted <- hospitals[order(hospitals$pneumonia, hospitals$Hospital.Name),]
  }
  
  if (inRank == "best") {
    inRank <- 1
  }
  else if (inRank == "worst") {
    inRank <- nrow(sorted)
  }
  
  if (inRank > nrow(sorted)) {
    return (NA)
  }
  
  return (sorted[inRank,]$Hospital.Name)
}