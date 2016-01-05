rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(outcome.data[,11] <- as.numeric(outcome.data[,11]))
  suppressWarnings(outcome.data[,17] <- as.numeric(outcome.data[,17]))
  suppressWarnings(outcome.data[,23] <- as.numeric(outcome.data[,23]))  
  ## Check that state and outcome are valid
   if (sum(as.numeric(c("heart attack","heart failure","pneumonia") == outcome)) == 0) {
    stop("invalid outcome")
  }  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  states <- sort(levels(as.factor(outcome.data$State)))
  ranks <- data.frame(cbind(rep("BLANK",length(states)),states), row.names = states)
  names(ranks) <- c("hospital","state")
  ranks$hospital <- as.character(ranks$hospital)
  for (i in 1:length(states))
  {
    if (outcome == "heart attack") {
      outcome.temp <- outcome.data[is.na(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 0 & outcome.data$State == states[i],]
      outcome.temp <- outcome.temp[order(outcome.temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome.temp$Hospital.Name),]
      if (num == "best") num.temp <- 1
      else if (num == "worst") num.temp <- nrow(outcome.temp)
      else num.temp <- num
      ranks[i,1] <- outcome.temp$Hospital.Name[num.temp]
      
    }
    else if (outcome == "heart failure") {
      outcome.temp <- outcome.data[is.na(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 0 & outcome.data$State == states[i],]
      outcome.temp <- outcome.temp[order(outcome.temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome.temp$Hospital.Name),]
      if (num == "best") num.temp <- 1
      else if (num == "worst") num.temp <- nrow(outcome.temp)
      else num.temp <- num
      ranks[i,1] <- outcome.temp$Hospital.Name[num.temp]

    }
    else if (outcome == "pneumonia") {
      outcome.temp <- outcome.data[is.na(outcome.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 0 & outcome.data$State == states[i],]
      outcome.temp <- outcome.temp[order(outcome.temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome.temp$Hospital.Name),]
      if (num == "best") num.temp <- 1
      else if (num == "worst") num.temp <- nrow(outcome.temp)
      else num.temp <- num
      ranks[i,1] <- outcome.temp$Hospital.Name[num.temp]
    }
  }
  ranks
  
}
