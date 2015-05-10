rankhospital <- function(state, outcome, num = "best"){
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
                     
                     ##Return hospital name in that state with lowest 30-day death rate
                     finalOutcome <- aux[order(as.numeric(aux[,myCol]), aux$Hospital.Name),]
                     if(num == "best"){
                            finalOutcome$Hospital.Name[1]
                     }
                     else if(num == "worst"){
                            finalOutcome$Hospital.Name[nrow(finalOutcome)]
                     }
                     else{
                            finalOutcome$Hospital.Name[num]
                     }
              }
              else{
                     stop("invalid outcome")
              }
       }
}