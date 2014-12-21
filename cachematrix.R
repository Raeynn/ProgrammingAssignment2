## makeCacheMatrix makes adds new functions to a matrix to cache the inverse
## calculations. cacheSolve checkes for a cached value to return before
## calculating the inverse and assigning the value to the cache.

# Example usage:
# > x <- matrix(rnorm(5), nrow = 5)           // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create a special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse


## This function creates a vector which is a list containing functions 
## to get and set the value of the vector 'x' and get and set the value 
## of the inverse matrix 'inv'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix passed as x
## If inverse already available in the cache, this value is returned, otherwise
## the function computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse if available, otherwise set as NULL
  inv <- x$getinverse()
  
  ## If cache is available, return the value and end function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  
  ## If cache is not available, continue function to calculate inverse 
  ## and add to cache
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
  }
}
