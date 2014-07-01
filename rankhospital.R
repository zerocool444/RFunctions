rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     ## Check that state and outcome are valid
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
     
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
     subData <- subset(outcomeData,toupper(State) == toupper(state),select=c(2,index))
     subData[,2] <- suppressWarnings(as.numeric(subData[,2]))
     subData <- subset(subData,!is.na(subData[,2]))
     colnames(subData) <- c("Hospital.Name","Rate")
     subData <- subData[order(subData[,2],subData[,1]),]
     for(i in 1:nrow(subData))
     {
          subData[i,3] <- i
     }
     colnames(subData)[3] <- "Rank"
     
     if(num == "best")
     {
          subData[1,1]
     }
     else if(num == "worst")
     {
          subData[nrow(subData),1]
     }
     else if(num > nrow(subData))
     {
          NA
     }
     else 
     {
          subData[num,1]     
     }
}