## Write two functions to cache the inverse of a matrix to reduce the need to repeatedly
## computing it and hence reducing the computation time

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSove
## should retrieve the inverse from the cache. The matrix here is assumed to be invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}