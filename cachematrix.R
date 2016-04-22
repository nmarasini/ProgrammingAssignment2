
# Developed following two functions will be used to cache matrix inverse.

# makeCacheMatrix is a function which creates a matrix or list object to cache its inverse
# The following steps will be performed while executing the makeCacheMatrix function

# 1. set  matrix value
# 2. get matrix value
# 3. set inverse of the matrix value
# 4. get inverse of the matrix value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(compute) inv <<- compute
  getinverse <- function() inv
  list(set=set,
       get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}


# The "cacheSolve" function generate the inverse of the matrix. In first step it checks whether
# the inverse has been already computed or not. If already computed, it retrieves the result and skips the
# computation. If not, it computes the inverse of matrix and sets the value in the cache


# This function assumes that the matrix is always invertible.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample test:
## > test1 = diag(3,5)
## > make = makeCacheMatrix(test1)
## > make$get()
##    [,1] [,2] [,3] [,4] [,5]
##    [1,]    3    0    0    0    0
##    [2,]    0    3    0    0    0
##    [3,]    0    0    3    0    0
##    [4,]    0    0    0    3    0
##    [5,]    0    0    0    0    3

## Cache will not be there while first time run
## > cacheSolve(make)
##    [,1]      [,2]      [,3]      [,4]      [,5]
##    [1,] 0.3333333 0.0000000 0.0000000 0.0000000 0.0000000
##    [2,] 0.0000000 0.3333333 0.0000000 0.0000000 0.0000000
##    [3,] 0.0000000 0.0000000 0.3333333 0.0000000 0.0000000
##    [4,] 0.0000000 0.0000000 0.0000000 0.3333333 0.0000000
##    [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.3333333

## Retrieving after the cache created from first run
## > cacheSolve(make)
##  getting cached data.
##  [,1]      [,2]      [,3]      [,4]      [,5]
##  [1,] 0.3333333 0.0000000 0.0000000 0.0000000 0.0000000
##  [2,] 0.0000000 0.3333333 0.0000000 0.0000000 0.0000000
##  [3,] 0.0000000 0.0000000 0.3333333 0.0000000 0.0000000
##  [4,] 0.0000000 0.0000000 0.0000000 0.3333333 0.0000000
##  [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.3333333
##  >