## The functions listed below work together to calculate the inverse of a matrix
## The first function creates a list containing a vector and four methods
## The second function checks for a cached copy of the inversed vector
## If it doesn't find a cached copy, it performes the calculation.

## The makeCacheMatrix function creates a list that contains a vector and four methods
##1. to set the value of the vector [set()], 
##2. get the value of the vector [get()],
##3. set the value of the inverse [setinverse()] , and
##4. get the value of the inverse [getinverse()]

makeCacheMatrix <- function(x = numeric()) {
  ## this sets the initial value of the inversed matrix = NULL
  m <- NULL
  
  ## the set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## the get function displays the value of x
  get <- function() x

  ## the setinverse function takes an input argument (x) and caches its inverse as m
  setinverse <- function(solve) m <<- solve
  
  ## the getinverse function displays the cached (inversed) value of m
  getinverse <- function() m
  
  ## the output of makeCacheMatrix is a list containing the four functions defined above (set, get, setinverse, and getinverse)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the vector created by the makeCacheMatrix function.
## The function also checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse method.
cacheSolve <- function(x, ...) {
  ## this looks for the inverse of x in the cache and stores it as m
  m <- x$getinverse()
  
  ## if m is not null, a message is displayed to clarify that a cached value of m is used and displayed
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if a cached value of m is not found, the inverse of x needs to be calculated.  This line gets the value of x.
  data <- x$get()
  
  ## the solve function cacluates the inverse of the data value x and stores as m
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## return the value of m
  m
}