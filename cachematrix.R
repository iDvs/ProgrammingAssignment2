## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
    
    m <- NULL        # set M NULL
    set <- function(y) {
        x <<- y      # Create obj. x = y in the function "set" environment
        m <<- NULL   # Create NULL m variable in func. 'set"
    }
    get <- function() x   # get x matrix
    setsolve <- function(solve) m <<- solve # set inverse matrix
    getsolve <- function() m                # get inverse matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)  # Create special matrix object with x, inverse X 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if ((!is.null(m))&(identical(m,x))) {  #check  calculating and difference
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
