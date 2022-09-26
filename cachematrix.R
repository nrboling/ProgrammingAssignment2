## Combined, the makeCacheMatrix and the cacheSolve function below take an
## invertible matrix, invert it, and then cache the inversion for future use

## The makeCacheMatrix takes a matrix and returns a list of functions which will
## perform a set of operations on the matrix which cache the results for future
## use.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a list of functions which are closures over a matrix and
## returns the inversion of the original matrix. If the inverse has been
## previously calculated it returns the cached result.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
