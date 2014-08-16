## Matrix Inversion: Cache
## Function Library: 
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. The "makeCacheMatrix" function generates the special "matrix" to be inverted 
## and defines the sub-functions to be used in order to cache a value/matrix
## The cacheSolve would then be used to compute the inverse of the special "matrix" returned by makeCacheMatrix or 
## cache the matrix if the inverse has already been calculated


## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL   
  
  # this function is used to set the value y into x and initializes the "inverse" matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # this function is used to get the value x 
  get <- function() x
  # this function is used to set the value "matrix" into the "inverse" in the global environment 
  setInvMatrix <- function(matrix) inverse <<- matrix
  # this function is used to get the value "inverse" from the global environment
  getInvMatrix <- function() inverse
  # return a list with the above sub-routines
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the inverse of the matrix and assign it to "mat"
  mat <- x$getInvMatrix()
  # If "mat" is already calculated then cache the value and return "mat"
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  # If "mat" is empty then get the matrix to be inverted and assign it to "data"
  data <- x$get()
  # Calculate the inverse of the "data" matrix and assign it to "mat"
  mat <- solve(data, ...)
  # Put the inverse "mat" matrix into the global environment. If the 'x' remains the same, this matrix "mat" 
  # would be sourced from the "inverse" value from "makeCacheMatrix" function defined above
  x$setInvMatrix(mat)
  mat
}
