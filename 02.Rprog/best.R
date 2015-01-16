best <- function(state, outcome)
{
    
    switch(outcome,
        "heart attack" = .bestX(11,state) ,
        "heart failure" = .bestX(17,state) ,
        "pneumonia" = .bestX(23,state) ,
        stop("invalid outcome"))

    
}

.bestX <- function(columnNumber,state)
{
    oData <- read.csv("outcome-of-care-measures.csv",colClasses="character")

    stateData = oData[oData$State == state,]
    if (nrow(stateData) == 0)
    {
        stop("invalid state")
    }

    stateData[,columnNumber] <- as.numeric(stateData[,columnNumber])

    ord = order(stateData[,columnNumber],stateData$Hospital.Name)
    stateData = stateData[ord,]
  
    return(stateData$Hospital.Name[1])
}

