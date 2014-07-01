rankall <- function(outcome, num = "best") {
     outcomeData <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
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
     subData <- subset(outcomeData,select=c(2,State,index))
     subData[,3] <- suppressWarnings(as.numeric(subData[,3]))
     subData <- subset(subData,!is.na(subData[,3]))
     colnames(subData)[3] <- "Rate"
     subData <- subData[order(subData$State,subData$Rate,subData$Hospital.Name),]
     subData$State <- factor(subData$State)
     subData <- do.call(rbind,lapply(split(subData,subData$State),function(x) cbind(x,seq(1:nrow(x)))))
     colnames(subData)[4] <- "Rank"
     
     if(num == "best")
     {
          output <- do.call(rbind,lapply(split(subData,subData$State),function(x) subset(x,x$Rank == min(x$Rank))))[,1:2]
          colnames(output) <- c("hospital","state")
          output
     }
     else if(num == "worst")
     {
          output <- do.call(rbind,lapply(split(subData,subData$State),function(x) subset(x,x$Rank == max(x$Rank))))[,1:2]
          colnames(output) <- c("hospital","state")
          output
     }
     else if(num > nrow(subData))
     {
          NA
     }
     else 
     {
          rankOnly <- do.call(rbind,lapply(split(subData,subData$State),function(x) subset(x,x$Rank == num)))[,1:2]  
          uniqueStates <- data.frame(unique(subData$State))
          colnames(uniqueStates) <- "State"
          rankOnly <- merge(uniqueStates,rankOnly,by="State",all.x=TRUE)
          output <- rankOnly[,c(2,1)]          
          colnames(output) <- c("hospital","state")
          output
     }
}