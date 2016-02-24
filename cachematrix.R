## Put comments here that give an overall description of what your
## functions do


## takes in a matrix and returns a list containing functions to
## set the matrix, get the matrix, set the inverse, and get the inverse.
## This list is passed along to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invert) inv <<- invert
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## checks to see if inverse has been calculated. If it has then it retrieves
## it from cache and returns it. If it has not been calculated, then cacheSolve
## calculates it and stores it in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #checks to see if inverse has been calculated
  if(!is.null(inv)){
    print("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv = solve(data, ...)
  return(inv)
}
