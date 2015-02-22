## A pair of functions that cache the inverse of a matrix.

##1.set value of matrix
##2.get value of matrix
##3.set the value of the inverse of the matrix
##4.get the value of the invserse

##Creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setresult <- function(solve) inverse <<- solve
  getresult <- function() inverse
  list (set = set, get = get, setresult = setresult, getresult = getresult) 
}


## This functions computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix above. If the inverse has 
## already been calculated (and matrix has not changed), 
## this should retrieve the inverse from the cache
## Note:Matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getresult()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setresult(inverse)
  inverse
}
