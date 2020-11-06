## Put comments here that give an overall description of what your
## functions do

## this function creates matrix object that can be inverted

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() {x}
      set_inverse <- function(inverse){inv <<- inverse}
      get_inverse <- function(){inv}
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## function computes the inverse of the function above ... special "matrix".  
# If inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from  cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
          if(!is.null(inv)){
            message("getting ccache data")
            return((inv))
          }
        mat <- x$get()
        inv <- solve(mat,...)
        x$set_inverse(inv)
        inv
              
}
