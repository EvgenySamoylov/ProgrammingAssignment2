## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
##
## cacheSolve function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix function creates a list object 
## of the length 4 which then will be an argument 
## of the cacheSolve function.
##
## The argument of the makeCacheMatrix should be a square
## and invertable matrix, otherwise the cacheSolve function
## will return an error.

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


## cacheSolve function returns a matrix that is the inverse of 'x'.
## If it was already calculated previously, the result will be
## returned from the cache.

cacheSolve <- function(x, ...) {
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
