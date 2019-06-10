## This function creates a special "matrix" that will cache the inverse
## Set and get the value of the mean
## Defining functions 
makeCacheMatrix <- function(x = matrix()) {
      invertmat <- NULL
      set <- function(y){
            x <<- y
            invertmat <<- NULL
      }
      get <- function() x
      setinverse <- function(mean) invertmat <<- mean
      getinverse <- function() invertmat
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## This function computes the inverse of the special "matrix". cacheSolve will retrieve 
## matrix data. 

cacheSolve <- function(x, ...) {
      invertmat <- x$getinverse()
      if(!is.null(invertmat)) {
            message("getting cached data")
            return(invertmat)
      }
      data <- x$get()
      invertmat <- solve(data, ...) ## Using solve function
      x$setinverse(invertmat)
      invertmat
}


