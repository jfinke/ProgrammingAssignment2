## These functions work together to create a special caching algorithm for 
## caching the inverse of a matrix

## This function sets out the original matrix using a vector.  It stores four
## values which can be accessed via the second function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function calculates the inverse of the matrix.  If the matrix has not been
## changed and it has already been calculated then it returns a cached value of 
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
