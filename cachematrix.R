## makeCacheMatrix and cacheSolve take a matrix as input and produces
## the inverse of that matrix as the output

## makeCacheMatrix sets and gets the inverse of a matrix and also
## gets and sets the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    #calculates the inverse
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve call solve to retrieve the inverse of a matrix checking
## the inverse exist in which case use the cache version. As output
## print the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse
    ##check if it is possible to use the cache version
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    ##print the inverse of the matrix
    i
}
    