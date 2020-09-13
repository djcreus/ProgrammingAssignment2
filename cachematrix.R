
## Stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) j <<- Inverse
  getInverse <- function() j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setInverse(j)
  j
}


B <- matrix(c(1,2,3,4),2,2)
#solve(B) #We pretend that this cant't happen xD

B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation

cacheSolve(B1)