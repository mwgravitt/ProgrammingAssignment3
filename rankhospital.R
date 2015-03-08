
# abstracted these basic operations that take place regardless of the condition
# subset the data and sort 
subsetandsort <- function (inHospitals, inState, inColumnNum) {
  
  # coerce the column to numeric - required to remove missing values
  inHospitals[[inColumnNum]] <- suppressWarnings(as.numeric (inHospitals[[inColumnNum]]))
  
  # subset to just a single state and remove NA
  inHospitals <- subset (inHospitals, !is.na(inHospitals[inColumnNum]) & inHospitals$State == inState)
  
  # sort by the appropriate column, then by hospital name
  sorted <- inHospitals[order(inHospitals[inColumnNum], inHospitals$Hospital.Name),]
  
  return (sorted)
}

rankhospital <- function(inState, condition, inRank = "best") {
  
  ## Read outcome data
  outcome <-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  validCondition <- c("heart attack", "heart failure", "pneumonia")
  columnNum <- c(11,17,23)
  conditionsMetadata = data.frame(validCondition, columnNum)
  
  # Check that outcome is valid
  if (!condition %in% conditionsMetadata$validCondition) {
    stop ("invalid outcome")  
  }

  # Check that state is valid
  if (!inState %in% outcome$State) {
    stop ("invalid state")  
  }
  
  sorted <- subsetandsort (outcome, inState, 
                           conditionsMetadata[conditionsMetadata$validCondition==condition,]$columnNum)
  
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