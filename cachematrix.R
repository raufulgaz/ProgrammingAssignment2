## R Programming - Assignment 2
## this R script contains two functions
## "makeCacheMatrix" that creates a cacheable matrix 
## "cacheSolve" that inverts a matrix

## makeCacheMatrix function
## 'x' is the matrix to be cached.
## return a list of four functions:
## - set: overwrite the matrix
## - get: returns the values of the matrix
## - setinv: input to the inverse of the matrix
## - getinv: returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  ##cleans variable
  inv <- NULL
  # set the matrix; remove cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  # set and get the inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  # return list of all the functions
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function
## inverts a matrix. 
## if a cached solution is found, it is returned; 
## otherwise, an inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
  # if cached inverse exists, return it
  if(!is.null(x$getinv())){
    message("returning cached solution.")
    x$getinv()
  }
  # otherwise, solve and cache new inverse
  else {
    message("cached solution not found ... new solution...")
    x$setinv(solve(x$get()))
    message("returning cached solution.")
    x$getinv()
  } 
  ## Return a matrix that is the inverse of 'x'
}
