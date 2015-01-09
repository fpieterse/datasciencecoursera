complete <- function(directory, id=1:332)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    nobsCol = vector(mode="integer",length=length(id))

	for (i in seq_along(id))
	{
        instrNum = id[i]

		# Use sprintf to format integer as zero-padded string
		filename = sprintf("%s/%03d.csv",directory,instrNum)
		
		# Read CSV for instrument into dataframe
		df = read.csv(filename)

        nobsCol[i] = sum(complete.cases(df))
		
	}
    
    data.frame("id"=id, "nobs"=nobsCol)
}


