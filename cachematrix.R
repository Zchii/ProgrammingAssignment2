## These two functions are caching the solution of matrix inverse. 
## To make the inverse compute fast at the second time.

## makeCacheMatrix function creates a special "vector", which is really a list containing a function to
## set the value, get the value of the matrix
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list( get = get,
         setinv = setinv, getinv = getinv)
}


## The following function calculates the mean of the special "vector" 
## created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
