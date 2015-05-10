best <- function(state, outcome){
       ## Read outcome data
       myFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       
       ##Check that state and outcome are valid
       myTestRows <- nrow(myFile[myFile[,7] == state,])
       if(myTestRows <= 0){
              stop("invalid state")
       }
       else{
            if(outcome == 'heart attack' || outcome == 'heart failure' || outcome == 'pneumonia'){
                   mySubsetState <- myFile[myFile[,7] == state,]
                   if(outcome == "heart attack"){
                          myCol <- 11
                   }
                   else if(outcome == "heart failure"){
                          myCol <- 17
                   }
                   else if(outcome == "pneumonia"){
                          myCol <- 23
                   }
                   subset <- is.na(as.numeric(mySubsetState[,myCol]))
                   aux <- mySubsetState[!subset,]
                   myLowestMortality <- min(as.numeric(aux[,myCol]))
                   finalOutcome <- subset(mySubsetState, as.numeric(mySubsetState[,myCol]) == as.numeric(myLowestMortality))
                   
                   ##Return hospital name in that state with lowest 30-day death rate
                   finalOutcome <- finalOutcome[order(finalOutcome$Hospital.Name),]
                   finalOutcome$Hospital.Name[1]
            }
            else{
                   stop("invalid outcome")
            }
       }
}