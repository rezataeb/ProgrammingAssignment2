## Name:Reza - Coursera - R - Assignment2

## This function tries to get a matrix and finds its inverse, However first
## it tries to find out whether it has done the same task on the same matrix or not
## if it finds the same matrix it just retrieve it and does not compute again. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache, so it does not spend time to calculate it again.
## But if it is a first time to enter this matrix, it will calculate the inverse of matrix.

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
        message("retrieving cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

## Return a matrix that is the inverse of 'x'
}
