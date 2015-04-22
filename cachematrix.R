## This function takes a matrix as its argument and will create a 
## list of functions in order to cache the matrix's inverse.
## The four functions allow to change the original matrix (in which case 
## the cached inverse gets reset), get the matrix that is stored,
## set the inverse of the matrix (which is what is being cached), and get 
## the inverse if it has been cached.

makeCacheMatrix <- function(x = matrix()) {
    ## i is the cached inverse of the matrix set to NULL the first time around
    i <- NULL 
    ## changes the matrix and resets the cache.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## returns the matrix
    get <- function() x
    ## caches the inverse
    setInverse <- function(inverse) i <<- inverse
    ## returns the inverse
    getInverse <- function() i
    ## returns a list of the functions to be able to access them using $
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Write a short comment describing this function
## This function first checks if the inverse of the matrix has been cached.
## If so, it returns a message "getting cached data" and returns the 
## inverse saved in makeCacheMatrix. Otherwise, it calculates the inverse,
## saves it in makeCacheMatrix, and returns it.

cacheSolve <- function(x, ...) {
    ## checks for cached inverse
    i <- x$getInverse()
    ## if there is a cached inverse it returns it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## otherwise it computes an inverse, caches it, and returns it.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
