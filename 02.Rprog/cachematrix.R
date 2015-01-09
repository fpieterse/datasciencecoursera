## Programming Assignment 2
##
# makeCacheMatrix returns a list of functions that will from here on be refered to
# as the cacheMatrix object. The cacheMatrix object has four functions:
# - setMatrix: sets the matrix used by the cacheMatrix object
# - getMatrix: returns the matrix
# - getInverse: returns the inverse of the matrix. The inverse is calculated the
#   first time it is required and cached from there on.
# - recalcInverse: Force recalculation of inverse.
# 
# Example:
#    > # create a cacheMatrix object
#    > cMatrix <- makeCacheMatrix(matrix(1:4,ncol=2))
#    > # calculate the inverse (should print message that shows it is calculating the
#    > # inverse
#    > inv = cMatrix$getInverse()
#    [1] "Calculating Inverse..."
#    > # calculate the inverse again (inverse calculation message should not print)
#    > inv = cMatrix$getInverse()
#    > # Test inverse
#    > print(inv*cMatrix$getMatrix())
#         [,1] [,2]
#    [1,]   -2  4.5
#    [2,]    2 -2.0
#    > print(inv%*%cMatrix$getMatrix())
#         [,1] [,2]
#    [1,]    1    0
#    [2,]    0    1



## makeCacheMatrix returns a list of functions.
## Functions allow caller to set/get a matrix as well as get the matrix inverse.
## Caller cannot set the matrix's inverse, the inverse is automatically
## calculated when required.
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL

    setMatrix <- function(newMatrix)
    {
        x <<- newMatrix
        inv <<- NULL
    }

    getMatrix <- function() x

    getInverse <- function()
    {
        # If inverse has not been calculated, inv will be null.
        if (is.null(inv))
        {
            print("Calculating Inverse...")
            inv <<- solve(x)
        }
        else
        {
            print("Using cached Inverse")
        }

        return(inv)
    }

    recalcInverse <- function()
    {
        # Set inv to NULL to force recalculation.
        inv <<- NULL
    }

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         getInverse = getInverse,
         recalctInverse = recalcInverse)
}


## cacheSolve calculates inverse of cacheMatrix.
##
## This function is not necesarry. I kept it here for the assignment to work.
## This function just calls the getInverse function on the cacheMatrix object
cacheSolve <- function(x, ...) {
    x$getInverse()
}
