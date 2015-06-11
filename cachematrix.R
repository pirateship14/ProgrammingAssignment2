## The first function (makeCacheMatrix) takes as input a matrix and
## creates a cache to store its inverse, the second function then
## takes the object returned by the first function and 
## checks cache for inverse before doing a new computation.

## makeCacheMatrix creates getters and setters to 
## add a matrix and its inverse, if computed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Sets the object's cache to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##cacheSolve first checks the cache of the object for a computed value
##If none is found, it proceeds with the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m) ##Set the cache to contain the inverse
  m
}
