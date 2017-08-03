## write a pair of functions that cache the inverse of a matrix，
## makeCacheMatrix is a special matrix containing functions and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <<- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      set_inverse <- function(inverse) inv <<- inverse 
      get_inverse <- function () inv
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)      
}


## compute the inverse for the matrix given by makeCacheMatrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$get_inverse()  ## Oh my god! don't forget the ()
      
      ## if inv is exist already
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      } 
      
      ## if inv is not exist
      data <- x$get()
      inv <- solve(data, ...)
      
      # Cache this result in the object
      x$set_inverse(inv)
      inv
}

# mat <- makeCacheMatrix(matrix(1:4,2))
# cacheSolve(mat)
# mat$get_inverse()


