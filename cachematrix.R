## These functions are used to cache inverse matrix calculations.
 # Calculating the inverse matrix with solve is a very time-consuming operation.
 # Existence of the inverse of a matrix is assumed.

## Return matrix object wrapper that supports inverse matrix cache.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse)  i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cache inverse matrix in case it is not already calculated and return cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
