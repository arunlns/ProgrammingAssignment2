## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse, which is really list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix(NA,nrow(x),ncol(x))
  setmat <- function(y = matrix()) {
    x <<- y
    inv <<- matrix(NA,nrow(x),ncol(x))
  }
  getmat <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# The following function cacheSolve() returns the inverse of the matrix addressed in the above 
# function. However, it first checks to see if the inverse has already been derived. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise,
# it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.na(sum(inv))) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
