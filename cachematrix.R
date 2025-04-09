## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list containing functions to:
##Set and get the matrix.
##Set and get the cached inverse of the matrix.
##This allows you to store a matrix and its inverse in a single object, and to retrieve the inverse later without recalculating it

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
       getinverse = getinverse)
}


## The cacheSolve function takes the object created by makeCacheMatrix as input, and then checks if the inverse has already been calculated and stored. 
##If so, it retrieves the stored inverse from the cache. 
##If not, it calculates the inverse, stores it in the cache, and returns the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
