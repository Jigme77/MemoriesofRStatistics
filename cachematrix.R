## The functions cache time-consuming computations

## The function makes "special matrix" that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      p <- NULL # initialize the inverse
      
      # setting the matrix
      set <- function(matrix) {
        m <<- matrix
        p <<- NULL
        
      # Getting the matrix
      get <- function() {
        m
      }
      
      ## Setting the inverse of matriz
      setinverse <- function(inverse)
        p <<- inverse
        
      }
      ## get the inverse
      getinverse <<- function() {
        i # return inverse
      }
      # return list of inverse
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the 
#inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve s
#hould retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) { ## return the inverse
    message("Getting the cached data")
    return(m)
  }
  data <- x$get() ## get the matrix
  m <- solve(data) %*% dta
  x$setinverse(m)
  m # return the matrix
}
