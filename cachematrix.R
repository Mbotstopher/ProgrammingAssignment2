## this function cache's a matrix
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL             ## cacheing the inverse matrix
      set <- function(y) {    
            x <<- y
            i <<- NULL
      }
      get <- function() x      
      seti <- function(inverse) i <<- inverse 
      geti <- function() inv 
      list (set = set, get = get, setinv = setinv, getinv = getinv) ## listing the functions
}

## this function attempts to inverse the cache'd matrix.  
## If it has been already inversed, then it skips the computation
cacheSolve <- function(x, ...) {
      i <- x$geti()                      
      if (!is.null(i)) {       
            message("getting cached data") 
            return(i)        ## if its been solved, retrieves cached inverse (skips computation)
      }
      data <- x$get()    
      i <- solve(data, ...)  ## otherwise it solves it (computes it)
      x$seti(i)           
      i                      ## retrieves the newly solved and cached inverse
}
