best <- function(state, outcome) {
     ## Read outcome data
     ## Check that state and outcome are valid
     ## Return hospital name in that state with lowest 30-day death
     ## rate
     states <- read.csv("states.txt")
     outcomeData <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
     if(length(states[states == toupper(state)]) == 0)
     {
          stop("invalid state")
     }
     if(tolower(outcome) != "heart attack" &
             tolower(outcome) != "heart failure" &
             tolower(outcome) !="pneumonia")
     {
          stop("invalid outcome")
     }
     
     columnNames <- colnames(outcomeData)
     
     if(tolower(outcome) == "heart attack")
     {
          columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
     }
     else if(tolower(outcome) == "heart failure")
     {
          columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
     }
     else if(tolower(outcome) == "pneumonia")
     {
          columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
     }
     
     index <- as.numeric(which(columnNames == columnName))
     subData <- subset(outcomeData,toupper(State) == toupper(state))
     subData[,index] <- suppressWarnings(as.numeric(subData[,index]))
     minValue <- min(subData[,index],na.rm = TRUE)
     minData <- sort(subset(subData,subData[,index] == minValue,select = "Hospital.Name"))
     minData[1,1]
}