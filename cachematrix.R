## Calculates inverse of matrix and store results in cache


## Function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL

      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}



## Function computes the inverse of the matrix from makeCacheMatrix.  If
## inverse already exists then the inverse should be returned from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
}
