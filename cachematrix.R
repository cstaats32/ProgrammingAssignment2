## This pair of functions, makeCacheMatrix and cacheSolve,
## will cache the inverse of a matrix rather than calculating
## it repeatedly.

## This function creates a special vector, which is really a list to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      v <- NULL
      set <- function(y) {
            x <<- y
            v <<- NULL
      }
      get <- function()x
      setinv <- function(solve) v <<- solve
      getinv <- function() v
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
  
}


## This function calculates the inverse of the "matrix"
## created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it "gets" the inverse from the cache and skips the computation.
## Otherwise, the inverse is calculated using the "solve" function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      v <- x$getinv()
      if(!is.null(v)) {
            message("getting cached data")
            return(v)
      }
      data <- x$get()
      v <- solve(data, ...)
      x$setinv(v)
      v
}
