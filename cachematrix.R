## These two functions work together to provide the inverse of a matrix
##.and includes creating a cache to store previously calculated inverse. Through cacheSolve,
## a check is first performed to see if the matrix had its inverse previously calculated (solved). 
## If not, the inverse is calculated using the solve() function and cached. If previously 
## calculated, a message is returned to indicate the inverse is being retrieved from the cache
## and the inverse returned. 

## makeCacheMatrix returns an object that is used by cacheSolve,
## containing functions for getting and setting matrices and their inverses 
## (as noted in the comments below next to the functions). A set function is also 
## included to allow the makeCacheMatrix to be reset with different values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(aMatrix){                 ## reset values of a makeCacheMatrix object
    x <<- aMatrix
    m <<- NULL
  }
  get <- function() x                       ## retrieves x  (from the parent env.)
  setInverse <- function(solve) m <<- solve ## sets inverse of matrix (m) into cache
  getInverse <- function() m                ## retrieves inverse of matrix from cache
  list(set = set,                           ## assigns each of these functions as an element
    get = get,                              ## within a list(), and returns it to the parent environment.
    setInverse = setInverse,
    getInverse = getInverse)
}


## cacheSolve first calls getInverse (defined in makeCacheMatrix) to see if the inverse  
## was previously calculated (using the cacheSolve function). If yes, it puts out a message
## and returns that inverse. If not, it calculates the inverse of the matrix
## and then 1) sets the inverse matrix into the cache and 2) returns the inverse matrix. 


cacheSolve <- function(x, ...) {
  m <- x$getInverse()                       
  if(!is.null(m)) {                         ## if a null value was not returned for m,
    message("getting cached data")          ## a message is printed and m returned
    return(m)
  }
  data <- x$get()                           ## (cacheSolve continues if no m was found)
  m <- solve(data, ...)                     
  x$setInverse(m)
  m
}
