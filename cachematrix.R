##The following function calculates the mean of the special “vector” 
##created with the above function. However, it first checks to see if
##the mean has already been calculated. If so, it gets the mean from
##the cache and skips the computation. Otherwise, it calculates the 
##mean of the data and sets the value of the mean in the cache via the
##setmean function.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

##This function computes the inverse of the special “matrix”
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

