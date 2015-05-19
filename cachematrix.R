## Put comments here that give an overall description of what your
## functions do


##makeCacheMatrix creates a special "matrix", wich is actually 
## a list containing 4 functions to
## 1. set the value of the matrix in a cache
## 2. get the value of the matrix from the cache
## 3. set the value of the Inverse in a cache
## 4. get the value of the Inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
          x <<- y
          inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv  
    list (set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
}




## cacheSolve computes the Inverse of a special "matrix"
## created by the function makeCacheMatrix above.

## If the Inverse has already been calculated, 
## and the matrix has not changed,
## cacheSolve retrieves it from the cache
## with the message "getting cached data"

## If the Inverse has not been calculated yet, 
## the function calculates it, then sets its value
## in the cache via the sentinverse function.



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()                   
    if(!is.null(inv)){
      message("getting cached data")    
      return(inv)                       
    }  
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
