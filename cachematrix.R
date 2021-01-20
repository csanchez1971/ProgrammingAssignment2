## In the current assignment, two different functions have been created.

## A first one called "makeCacheMatrix" that creates a list that allow
## us to store a cached value of a matrix and its inverse previously 
## calculated with the next function.

## A second function called "cacheSolve" that will perform the matrix inversion, although first
## will check if we have a cached solution before doing any further calculation. In case we have
## the inversed already cached, function will return the value with a message explaining that 
## cached values was returned

## Function "makeCacheMatrix" will cache the matrix and its solution into a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Function "cacheSolve" will first check if we have a cached solution before doing any 
## calculation and if not, will execute "solve()" function.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setSolve(m)
        m
}
