## This is a two function combo that work together. The first one is used to
## create the input matrix with caching for the inverse solution.
## The second function is the inverse (solve) operation on the matrix within
## the object that allows for storage the first time, or retrieval subsequent
## times.

## This assignment uses the <<- operator which can be used to assign a value to
## an object in an environment that is different from the current environment.
## Below are two functions that are used to create a special object that stores
## a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is
## a numeric vector and cache's its mean.

## The first function, makeCacheMatrix creates a special "vector", which is
## really a list containing a function to:
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse matrix
## * get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve

  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special object created
## with the "makeCacheMatrix" (above) function. However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
