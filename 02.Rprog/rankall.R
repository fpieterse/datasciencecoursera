rankall <- function(outcome, num="best")
{
    switch(outcome,
        "heart attack" = .rankX(11,num) ,
        "heart failure" = .rankX(17,num) ,
        "pneumonia" = .rankX(23,num) ,
        stop("invalid outcome"))
}


.rankX <- function(colNum,num)
{
    if (num == "best")
    {
        num = 1
    }

    oData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    oData[,colNum] <- as.numeric(oData[,colNum])

    stateData <- split(oData,oData$State)

    state <- names(stateData)
    hospital <- rep("",length(state))

    for (i in seq(length(stateData),1,by=-1))
    {
        df = stateData[[state[i]]]
        ord = order(df[,colNum],df$Hospital.Name)
        ordDf <- df[ord,c(2,colNum)]

        if (nrow(ordDf) == 0)
        {
            hospital[i] <- NA
            next
        }
        else if (num == "worst")
        {
            for (r in seq(nrow(ordDf),1,by=-1))
            {
                if (!is.na(ordDf[r,2]))
                {
                    hospital[i] <- ordDf$Hospital.Name[r]
                    break
                }
            }
            if (hospital[i] == "")
            {
                hospital[i] <- ordDf$Hospital.Name[1]
            }

            next
        }
        else if (nrow(ordDf) < num)
        {
            hospital[i] <- NA
        }
        else
        {
            hospital[i] <- ordDf$Hospital.Name[num]
        }
      
    }

    return(data.frame(hospital,state))
}
