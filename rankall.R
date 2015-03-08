
source ("rankhospital.R") 

rankall <- function (inCondition, inRank = "best") {
  
  outcome <-read.csv("outcome-of-care-measures.csv", colClasses="character")
  states <- sort (unique (outcome$State))
  hospitals = vector()
  
  for (i in 1:length(states)) {
    hospitalName <- rankhospital (states[i], inCondition, inRank)
    hospitals[i] <- hospitalName
  }
  
  results <- data.frame (hospitals, states)
  return (results)
}