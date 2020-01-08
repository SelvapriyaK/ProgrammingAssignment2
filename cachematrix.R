#The functions makeCacheMatrix and cacheSolve is used to cache the inverse of a square matrix.
#makeCacheMatrix: 

# Sets the value of the input matrix
# Gets the value of the input matrix
# Sets the inverse of the matrix
# Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) m <<- invert
  getinvert <- function() m
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
  
}


# cacheSolve is used to compute the inverse of the matrix using the solve function.
# The inverse is stored in object 'm' and cached using the setinvert function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinvert(m)
  m
}
