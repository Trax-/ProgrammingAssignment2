## This pair of functions are used to create a special matrix 
## object that stores a numeric matrix and caches's it's inverse
## using the scoping rules of R.

## This function creates the getters & setters 
## for the cached matrix it takes a matrix 
## and creates a list that allows storage and 
## retrival of the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {

    temp <- NULL
    
    set <- function(value) {
        
        # set the new value
        x <<- value
        # clear out the old 
        temp <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) temp <<- solve
    
    getinv <- function() temp
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse matrix and caches it
## If this function is called again with the same data it will
## return the cached inverse matrix rather than re-calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    temp <- x$getinv()
    
    if(!is.null(temp)) {
        
        message("retrieving cached matrix")
        
        return(temp)
    }
    
    data <- x$get()
    
    temp <- solve(data, ...)
    
    x$setinv(temp)
    
    temp
}
