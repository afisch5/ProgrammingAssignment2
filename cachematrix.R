## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#Create 2x2 matrix titled mtx
mtx <- matrix(c(1, 2, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
#Check that matrix was created
print(mtx)
#Call user defined functions using user defined matrix
print(cacheSolve(makeCacheMatrix(mtx)))
#Compare the output from the user defined functions to base R solve function on the same user defined matrix
print(solve(m))