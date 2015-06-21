## makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function (x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- Null
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following funciton calculates the inverse of the matrix.
## However, it first checks to see if the nverse has already been calculated.
## If so, it gets the result from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the cache via the setinverse function.

cacheSolve <- function(x, ...){
  inv <- x$getinverse ()
  if(!is.null(inv)){
    mseeage("getting cached data.")
    return (inv)
  }
  data <-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}

