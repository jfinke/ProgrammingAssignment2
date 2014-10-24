## These functions work together to create a special caching algorithm for 
## caching the inverse of a matrix

## This function sets out the original matrix using a vector.  It stores four
## values which can be accessed via the second function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #Setting the global enviornment
    m <<- NULL #Setting the global enviornment
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #Using the solve function to create the invertible matrix
  getinverse <- function() m
  list(set = set, get = get, ##Setting the overall values
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix.  If the matrix has not been
## changed and it has already been calculated then it returns a cached value of 
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { #Testing to see if the value has already been calculated
    message("getting cached data")
    return(m) #Breaking out of the function if the cache is valid
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m #Returning new inverse
}
