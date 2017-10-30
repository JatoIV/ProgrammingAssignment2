##  Functions that Cache the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinv <- function(solveMatrix) inverse <<- solveMatrix
  getinv <-  function() inverse
  list( set = set, get = get, 
        setinv = setinv, 
        getinv = getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
       inverse <- x$getinv()
       if(!is.null(inverse)){
         message("getting cached data")
         return(inverse)
       }
       info <- x$get()
       inverse <- solve(info)
       x$setinv(inverse)
       inverse
}
