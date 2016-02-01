## A pair of functions that cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() {
    x
  }

  setSolve <- function(solve) {
    m <<- solve
  }

  getSolve <- function() {
    m
  }

  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cached value is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
