## These two functions are used to calculate the inverse of a matrix. 
## They assume an invertible matrix is always passed in.  

## This function caches the inverse of the matrix provided.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        # set function
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get function
        get <- function() x
        
        #setinverse
        setinverse <- function(inverse) i <<- inverse
        
        #getinverse
        getinverse <- function() i
        
        #return list with matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of a matrix
## Before performing the calculation, the function checks the cache to determine whether
## the inverse is already available before performing the calculation

cacheSolve <- function(x, ...) {
        ## check if inverse is available
        i <- x$getinverse()
        
        ## if available, return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## if still in the function, caclulate the matrix using 'solve'
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
