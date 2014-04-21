## makeCacheMatrix and cacheSolve take a matrix as input and produces
## the inverse of that matrix as the output

## makeCacheMatrix sets and gets the inverse of a matrix and also
## gets and sets the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    ##set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## retrieve or get  the matrix
    get <- function() x
    ##set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    ## retrieve or get  the invserse of the matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve call solve to retrieve the inverse of a matrix checking
## the inverse exist in which case use the cache version. As output
## print the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Atempt to retrieve the inverse of x
    i <- x$getinverse()
    ##check if it is possible to use the cache version
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## gets or retrieves the data
    data <- x$get()
    #calculates the inverse
    i <- solve(data,...)
    x$setinverse(i)
    ##print the inverse of the matrix
    i
}
    