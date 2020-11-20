## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse. It returns a list of get, set of both the input matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     # initialize m
  set <- function(y) {
    x <<- y     # the input matrix y is set to x in the parent environment
    m <<- NULL  # when a new matrix is 'set' to x, the value of m must be reset, where m is a cached value calculated by cachemean()
  }
  get <- function() x   # retrieve the value of x, which is the input matrix
  setinverse <- function(solve) m <<- solve # <<- assign value of inversed matrix to m in parent scope
  getinverse <- function() m                # retrieve the value of m: the solved and cached inverse matrix
  
  # a list of above functions as named elements is returned, $ can be used to call them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # if inverse is already calculated, it is stored in 'special matrix x', retrieve it by getinverse
  m <- x$getinverse() # m is either null (if not in cache), or the inverse (if in cache)
  
  # check if m was found, if so, the inverse matrix found from cache can be returned
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()       # get the input matrix from the special matrix object
  m <- solve(data, ...) # calculate the inverse of input matrix, store as m
  x$setinverse(m)       # store the result in cache for future reference
  m                     # also print the result
}