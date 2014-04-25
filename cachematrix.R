## Function makeCacheMatrix takes a matrix and makes
## a list containing four functions set,get,setinverse,
## and getinverse and function cacheSolve takes such
## a list and compute the inverse of the matrix only
## if it has not been put in setinverse yet.


## Each function is doing exactly what its name is! 
## Every assignment is done using <<- operator.

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

## We first check whether the inverse is already computed
## if yes, we return the value from cached data if not we
## compute and put the inverse in cache and return the inverse

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
