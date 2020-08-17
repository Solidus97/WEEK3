

## This function will prepare the variables in order to inverse the matrix on the next function

makeCacheMatrix <- function(x = matrix()) 
  { 
  j <- NULL
set <- function(y)
  {
  x <<- y
  j <<- NULL

}
get <- function()x
setInverse <- function(inverse) j <<- inverse
getInverse <- function() j 
list(set = set, get = get, 
     setInverse = setInverse, 
     getInverse = getInverse)
}


## This function actually computes and gets the inverse of the matrix

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
