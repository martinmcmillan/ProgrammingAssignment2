# These are a pair of functions that cache the inverse of a matrix since the inversion operation 
# is computationally costly in general.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
           x <<- y
           i <<- NULL
         }
	 
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
	  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


# This function computes and returns the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheolve retrieves
# the inverse from the cache.

# Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  i <- x$getinv()
	
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
    }
	
  data <- x$get()	
  i <- solve(data, ...)	
  x$setinv(i)	
  i
}
