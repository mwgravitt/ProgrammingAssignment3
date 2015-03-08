
source ("rankhospital.R") 

rankall <- function (inCondition, inRank = "best") {
  
  outcome <-read.csv("outcome-of-care-measures.csv", colClasses="character")
  state <- sort (unique (outcome$State))
  hospital = vector()
  
  for (i in 1:length(state)) {
    hospitalName <- rankhospital (state[i], inCondition, inRank)
    hospital[i] <- hospitalName
  }
  
  results <- data.frame (hospital, state)
  return (results)
}