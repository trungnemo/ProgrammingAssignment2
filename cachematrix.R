## By  : Tao Van Trung , email trungnemo@gmail.com 
## Date: 2025 Jan 23
## ToDo: Complete the R Programming Assignment 2

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Creates a matrix that can cache it's inverse
#
# Args:
#   x: A matrix (Optional), if no argument, then a empty matrix is created by default
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  # im cached inverse of matrix
  im <- NULL
  # getter for matrix
  get <- function() x      
  #setter for matrix
  set <- function(y){
    x <<- y
    im <<- NULL
  } 
  #Set the inverse of the matrix 
  setInverse <- function(solve) im <<- solve
  getInverse <- function() im
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##       If the inverse has already been calculated (and the matrix has not changed), 
##       then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  # check the inversed matrix
  im <- x$getInverse()
  # if im is not null then matrix has been cached/inversed already, return cached data
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  #if not, get the matrix and inverse values
  matrix <- x$get()
  im <- solve(matrix, ...)
  x$setInverse(im) #cache it
  # Return an inversed matrix of 'x'
  im 
}
