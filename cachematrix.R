## the following two functions creates the inverse of a matrix and store the
## matrix in cache

##this function calculates the inverse  of a matrix
# makeCacheMatrix creates a list containing
# the matrix
# its cache
# the  inverse of the matrix
# the cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {


  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function returns the inverse of the matrix. 
#first checks if the inverse has already been computed.

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
