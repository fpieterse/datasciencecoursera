corr <- function(directory, threshold = 0)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations


    filenames = list.files(directory)
    len = length(filenames)
    numObs = vector(mode="integer",length=len)
    corelations = vector(mode="numeric",length=len)


    for (i in seq_along(filenames))
    {
        df = read.csv(sprintf("%s/%s",directory,filenames[i]))
        numObs[i] = sum(complete.cases(df))
        if (numObs[i] > 0)
        {
            corelations[i] = cor(df['nitrate'],df['sulfate'],use="complete.obs")
        }
    }

    corelations[numObs>threshold]
}
