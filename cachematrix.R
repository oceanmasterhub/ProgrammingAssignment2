## -------------
## cachematrix.R
## -------------
## This file includes functions to speed up computation of the inverse of a
## matrix by means of a cache version stored in the environment

## -----------------
## makeCacheMatrix()
## -----------------
## This function creates a special list object with functions that will help
## cacheSolve() to retrieve the inverse of matrix 'x' from cache (if exists)

makeCacheMatrix <- function(x = matrix()) {
  
  inv     <- NULL
  set_mat <- function(y) {
    x    <<- y
    inv  <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  
  list(set_mat = set_mat, # Create object as list
       get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)
}

## ------------
## cacheSolve()
## ------------
## This function retrieves the inverse of a matrix from the cache if available
## and execute the solve() function instead when cache is empty

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  data <- x$get_mat()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
