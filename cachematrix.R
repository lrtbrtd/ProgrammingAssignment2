## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() function is providing services to :
## set() : set a matrix based on input values
## get() : get matrix values
## setinverse() set the corresponding inverse matrix and store it in a cached variable m
## getinverse() : get the eventual cached stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      ## init m 
      m <- NULL
      ## set up x coresponsing to y in put values
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      ## retrieve x values 
      get <- function() x
      ## use solve() function to find the inverse matrix of x, and eventually store it in m as a cached variable
      setinverse <- function(solve) m <<- solve
      ## retrieve stored inverse matrix
      getinverse <- function() m
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve() function tries to retrieve the inverse matrix form the m cached variable, and if not existing compute
## the inverse matrix using x$setinverse() function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      ## try retriveving  the inverse matrix 
      m <- x$getinverse()
      
      ## if inverse matrix found, a msg is displayed as well as the values of the inverse matrix 
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      ## if not, the matrix values of x are retrieved and the inverse matrix is computed and stored, for the next time to be
      ## found in m variable
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
