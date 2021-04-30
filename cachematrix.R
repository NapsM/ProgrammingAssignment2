## This function creates a special vector within another vector


makeCacheMatrix <- function(x = matrix()) {
              i <- NULL               ## The inverse will initially be null
              set <- function(y) {
                      x <<- y
                      i <<- NULL
              }
              get <- function() {x}     ## This function allows for obtain a matrix
              setInverse <- function(inverse) {i <<- inverse}
              getInverse <- function() {i}
              list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Retrieves the inverse

cacheSolve <- function(x, ...) {    ## Function allows for other functions to be written in the function
        i <- x$getInverse()
        if(!is.null(i)) {           ## Functions to call inverse matrix if it is not null and message will pop up if that is true
                    message("getting cached data")
                    return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)               ## Output should give inverse of matrix
        i                             ## Return a matrix that is the inverse of 'x'
}
