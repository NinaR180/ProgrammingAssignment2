## Put comments here that give an overall description of what your
## functions do

## This function takes in an input and converts it to a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
        )
}


## This function inverts the matrix created in the last function. If
## this inversion has already been done, it skips this step. 
## Otherwise, it calculates the inverse of the data matrix and
## sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
     message("Here is the cached data:")
     return(m)
  }
  data <- x$get
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
