## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## initialise variables
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## get contents of x
    get <- function() {
      x
    }
    
    ## create inverse and put into m
    setinverse <- function(solve) {
      m <<- solve
    }
    
    ## retrieve value of m
    getinverse <- function() {
      m
    }
    
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {

  ## try to get the cached inverse from m
  m <-x$getinverse()
  
  ## if m contains data, return it ...
  if(!is.null(m)){
    message("Returning cached data")
    return(m)
  }
  
  ##  ... otherwise create a temporary matrix and 'solve' it
  data <-x$get()
  m <- solve(data)

  ## then cache the inverse for next time round
  x$setinverse(m)

  ## finally return  the inverse
  m
}
