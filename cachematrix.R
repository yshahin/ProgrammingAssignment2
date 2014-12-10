## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special metrix with the capability of setting
## and getting a matrix as well as getting and setting
## a cahce to store  the inverse of a matrix
## we can access the inverse using the getinverse and
## set inverse methods
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This custom solve method looks into the custom
## matrix that we created in the makeCacheMatrix function
## and looks to see if it has an inverse cached in it
## if there is one then we return it, else we calculate
## the inverse and use the setinverse on the custom matrix
## to set the caluclated inverse for future use (cached)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
