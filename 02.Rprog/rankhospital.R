rankhospital <- function(state, outcome, num)
{
    
    switch(outcome,
        "heart attack" = .rankX(11,state,num) ,
        "heart failure" = .rankX(17,state,num) ,
        "pneumonia" = .rankX(23,state,num) ,
        stop("invalid outcome"))

    
}

.rankX <- function(columnNumber,state,num="best")
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

    if (num == "best")
    {
        num = 1
    }
    
    if (num == "worst")
    {

        for (i in seq(nrow(stateData),1,by=-1))
        {
            if (!is.na(stateData[i,columnNumber]))
            {
                return(stateData$Hospital.Name[i])
            }
        }

        num = 1
    }

    if (nrow(stateData) < num)
    {
        return(NA)
    }
  
    return(stateData$Hospital.Name[num])
}

