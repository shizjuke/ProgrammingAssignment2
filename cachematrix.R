## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize 'inv'
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    ## if new matrix is set then erase the inverse value
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of a matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse of a matrix
  getinverse <- function() inv
  
  ## return special "matrix"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## check if the inverse of a matrix has already been calculated
  if(!is.null(inv)) {
    
    message("getting cached inverse")
    
    ## if the inverse of a matrix was calculated then return cached value
    ## and skip the computation
    return(inv)
  }
  
  ## if the inverse of a matrix is NULL then get matrix
  mtx <- x$get()
  
  ## and calculates the inverse 
  inv <- solve(mtx, ...)
  
  ## sets the value of the inverse in the cache
  x$setinverse(inv)
  
  ## return the inverse of a matrix
  inv
}
