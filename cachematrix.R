## These two functions together achieve the purpose to cache
## the inverse of a given matrix so that it doesn't need to be
## recomputed.

## This function make a special object which is a list 
## containing four functions: set the value of the matrix,
## get the value of the matrix, set the inverse matrix and 
## get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## This function solves the inverse matrix of the special
## object given by the first funciton. It checks whether
## there is a inverse matrix already in the list, then it
## will skip the computation process. Otherwise, it will
## compute the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
