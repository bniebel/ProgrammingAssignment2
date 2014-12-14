## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list that contains functions
##1. to set the value of the vector [makeCacheMatrix()], 
##2. get the value of the vector [get()],
##3. set the value of the mean, and
##4. get the value of the mean
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(x) m <<- -x
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  m<- -x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
