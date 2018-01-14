## The inverse of a matrix is cached which saves computation time when the inverse of a matrix
## is needed repeatedly (eg in a loop).
## the makeCacheMatrix function creates an object in which a matrix and its inverse can be hold.
## cacheSolve function returns the inverse of a matrix.

## The makeCacheMatrix function creates a matrix and calcutales the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the inverse matrix created 
## by the makeCacheMatrix function if inverse not already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
