rankall <- function(outcome, num = "best"){
  ## Read outcome data
  myFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Check that outcome are valid
  if(outcome == 'heart attack' || outcome == 'heart failure' || outcome == 'pneumonia'){
      if(outcome == "heart attack"){
        myCol <- 11
      }
      else if(outcome == "heart failure"){
        myCol <- 17
      }
      else if(outcome == "pneumonia"){
        myCol <- 23
      }
      
      myFile <- suppressWarnings(myFile[order(myFile$State, as.numeric(myFile[,myCol]), myFile$Hospital.Name),])
      
      ## For each state, find the hospital of the given rank
      subset <- split(myFile, myFile$State)
      
      if(num == "best"){
        finalOutcome <- lapply(subset, function(i) i[1,])
      }
      else if(num == "worst"){
        finalOutcome <- lapply(subset, function(i) i[nrow(i),])
      }
      else{
        finalOutcome <- lapply(subset, function(i) i[num,])
      }
      
      hospitales = vector()
      estados = vector()
      for(f in finalOutcome){
        hospitales <- append(hospitales, f[,2])
        estados <- append(estados, f[,7])
      }
      frameFinal <- data.frame(hospitales, estados)
      colnames(frameFinal) <- c("hospital","state")
      return(frameFinal)
    }
  else{
    stop("invalid outcome")
  }
}