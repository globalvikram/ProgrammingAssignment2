## Below functions makeCacheMatrix and cacheSolve matrix is created for computing inverse
## of matrix using the cache.
## Sample i/p to run the below objects:
## a <- makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## a$get()
## cacheSolve(a)

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i  <- NULL
  # To set the i/p matrix
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  # To retrive the i/p value
  get  <- function() {
    x
  }
  # To set the inverse of matix, called from cacheSolve
  setinverse  <- function(inverse){
    i  <<- inverse
  }
  # To retrive the inverse, called from cacheSolve
  getinverse  <- function(){
    i
  }
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## cacheSolve function will computes the inverse of i/p matrix returened by makeCacheMatrix
## If the inverse of the i/p matrix have been calculated earlier then,
## the it will retrieve the inverse from the cache and returns 

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
