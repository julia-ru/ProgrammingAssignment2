# This functions first creates a matrix that can store its inverse
# and later checks wether or not its inverse has already been calculated
# or needs to be calculated now.

# makeCacheMatrix() creates a matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # assigns the variables so that they can be used in other environments
    # then this function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # gets the original matrix
    get <- function() x
    # sets the inverse of the original matrix
    setinverse <- function(solve) m <<- solve
    # gets the inverse of the original matrix
    getinverse <- function() m
    # creates output with getter, setter, getinvers and setinverse variables
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve() computes the inverse of the special "matrix" returned by
# makeCacheMatrix above; if the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve retrieves 
# the inverse from the cache
cacheSolve <- function(x, ...) {
    # checks wether or not the inverse has already been calculated
    m <- x$getinverse()
    # if it has already been calculated, its getting the cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if it hasn't been calculated, it gets the original matrix and 
    # calculates its inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
