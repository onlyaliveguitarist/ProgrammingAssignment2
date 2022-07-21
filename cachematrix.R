## the first function creates a matrix with the ability to cache its inverse

## this sets up a list with functions set, get, setinverse, and getinverse
## that makes a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## calculates the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cachesolve will retrieve
## the inverse from the cache using getinverse, otherwise it will calculate
## it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
