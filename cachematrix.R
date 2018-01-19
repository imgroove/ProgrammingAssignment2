## Here are some steps that might be helpful in reviewing the code
## You can run through these steps and read the comments to see what I expect to happen

rm(list=ls()) # clear environments

source("CacheMatrix.R")  # load up the 2 required assignment functions

# make 2 matrices to test switching data
a<-matrix(c(2,4,7,9,7,4,2,4,9),3,3) # use to calculate inverse the first time
b<-matrix(c(4,6,3,2,3,6,7,8,3),3,3) # use 2nd try to test inverse recalculation

mcmResult<-makeCacheMatrix(a) # create functions and initialize inv
mcmResult$getinv()     # at the point the inv returns NULL
cacheSolve(mcmResult)  # run the inverse the first time
cacheSolve(mcmResult)  # run again and note the "cached" message
mcmResult$setData(b)       # reset the source data matrix
cacheSolve(mcmResult)  # New source data so no "cached" message
cacheSolve(mcmResult)  # run again with new data results in "cached" message



##***********************************************************
##  Here are the two required functions
## **********************************************************

# This is an edited version of the sample code provided in the exercise instructions.
# makecacheMatrix initializes the inv object, creates 4 behavior functions and
# puts them in a list that makes them available to the parent environment with 
# $ name style references

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setData <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getData <- function() x
        setinv <- function(inv) inv <<- solve(mcmResult$getData())
        getinv <- function() inv
        list(setData = setData, getData = getData,
             setinv = setinv,
             getinv = getinv)
}

# cachecSolve test to see if the inv is not NULL. TRUE retrieves the inv directly
# FALSE recalculates inv.  Note the messages identifying which case fired. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getData()
        inv <- solve(data, ...)
        message("inv not NULL - retrieving precalcuated data")
       
        x$setinv(inv)
        inv
}
