## A pair of functions that cache the inverse of a matrix.
## Note: Matrix supplied is always invertible.

##Creates a special "matrix" object that can cache it's inverse
##Steps:
##1.set value of matrix
##2.get value of matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse

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


## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix above. If the inverse has 
## already been calculated (and matrix has not changed), 
## this should retrieve the inverse from the cache.

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

## a sample for this is... 
## x <- rbind(c(1,2,3),c(0,1,4),c(5,6,0))
##result should be:
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1