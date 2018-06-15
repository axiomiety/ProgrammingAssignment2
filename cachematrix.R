## Caching logic for matrix-based computations

## Returns an object wrapping the underlying matrix for the purpose
## of caching certain properties of said object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## if the matrix changes, invalidate the inverse
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the cached return value of `solve` if it has been computed previously
## or calculates it and caches the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) return(inv)
  matrix_ <- x$get()
  inv <- solve(matrix_)
  x$setinverse(inv)
  inv
}
