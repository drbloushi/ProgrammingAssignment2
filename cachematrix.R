


## here we created the makeCacheMatrix,as the 1st function
## which makes the cache

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            
            x <<- y
            inverse <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) inverse <<- inverse
      getinverse <- function() inverse
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
      
      
}




## here the 2nd function which is getting the cache or inverse of a matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if (list.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      mat <- x$get()
      inverse <- solve(mat, ...)
      x$setInverse(inverse)
      inverse
        
}

