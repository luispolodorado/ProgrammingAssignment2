
##  makeCacheMatrix creates a special "matrix", which is really a list containing four functions:
##
## set the value of the "matrix"
## get the value of the "matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     inverseM <- NULL
     set <- function(y) {
          x <<- y
          inverseM <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inverseM <<- solve(x)
     getinverse <- function() inverseM
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
     inverseM <- x$getinverse()
     if(!is.null(inverseM)) {
          message("getting cached data")
          return(inverseM)
     }
     data <- x$get()
     inverseM <- solve(data, ...)
     x$setinverse(inverseM)
     inverseM
}
