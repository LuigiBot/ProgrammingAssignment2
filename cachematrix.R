## Put comments here that give an overall description of what your
## functions do

## Creates a list containing get,set,getinv,setinv in order to be able
## to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    invcache <- NULL
    set <- function(y){
      x <<- y
      invcache <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invcache <<- inv
    getinv <- function() invcache
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the matrix, used in conjunction with makeCacheMatrix
## Returns the cached inverse if it exists, or else it computes it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
