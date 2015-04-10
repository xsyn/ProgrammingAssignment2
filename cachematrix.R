## cachematrix.r 
##
## makeCacheMatrix: creates special matrix for caching purposes
##
## cacheSolve: returns calculation of cached inversion of a matrix.

## makeCacheMatrix(x) --
## creates a special matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion of the matrix
## 4. get the value of the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set Methods
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) <<- solve
  getInverse <- function() m
  
  # Return a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(x) --
## calculates the inversion of a matrix. It first checks to see if a cached version exits (m)
## if so it returns that cached version. Otherwise it calculates the what the inversion should be and creates
## a cached copy.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Check if cached version exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Create and return cached inversion of matrix
  matrix <- x$get()
  m <- solve(matrix)
  x$setInverse(m)
  m
}
