## function to inverse matrix and cache if matrix was already inverted


## function sets solve function for cacheSolve and clears cache

makeCacheMatrix <- function(x = matrix()) { 
      m <- NULL 
      set <- function(y) { 
            x <<- y    
            m <<- NULL  
      }
      get <- function() x 
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## function returns the inverse of a Matrix or retursn the cached matrix.
#Input agrument has to be of class makeCacheMatrix

cacheSolve <- function(x, ...) {
      m <- x$getsolve () 
      if(!is.null(m)) { 
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
