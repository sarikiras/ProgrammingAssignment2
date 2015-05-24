
##makeCacheMatrix creates a special "matrix", wich is actually 
## a list containing 4 functions to
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the Inverse 
## 4. get the value of the Inverse 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){         ## this function allows the matrix in the cache
          x <<- y
          inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse ##this function puts 
                                                    ##the inverse in the cache
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
  
  ## Return a matrix that is the inverse of a matrix M
  ## The argument 'x' is the special "matrix" makeCacheMatrix(M)
  
    inv <- x$getinverse()   ## Get the 'value' of the cached inverse
                            ## via the function setinverse
                                         
    if(!is.null(inv)){      ## Return the value if it has been calculated
      message("getting cached data")    
      return(inv)                       
    }                       
                          
        ## If the inverse has not been calculated yet:
    
    data <- x$get()       ## Get the initial matrix M via the function get
    inv <- solve(data,...)## Calcultate the inverse
    x$setinverse(inv)     ## Put the inverse in the cache via the function setinverse
    inv
}
