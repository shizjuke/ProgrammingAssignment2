## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## This special "matrix" is really a list containing a function to:
## 1. set the value of the matrix ('set')
## 2. get the value of the matrix ('get')
## 3. set the value of the inverse of a matrix ('setinverse')
## 4. get the value of the inverse of a matrix ('getinverse')

makeCacheMatrix <- function(x = matrix()) {
  
  ## cached inverse variable
  inv <- NULL
  
  ## define functions: set, get, setinverse, getinverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## return special matrix
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by 'makeCacheMatrix' above. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  
  ## return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## check if the inverse was calculated
  if(!is.null(inv)) {  
    message("getting cached inverse")
    ## return cached inverse
    return(inv)
  }
  
  ## calculate, cache and return the inverse
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setinverse(inv)
  inv
}
