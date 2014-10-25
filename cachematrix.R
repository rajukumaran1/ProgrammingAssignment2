## The functions in this file will save the matrix and its inverse. 
## CacheSolve function has to be called in order to avoid the recomputing inverse multiple times.

## This function creates a list, which returns functions 
## to access the matrix and inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  
  if(class(x)!="matrix")
  {
    print ("Invalid matrix")
    return(NULL)
  }
  
  m <- NULL
  set <- function(y = matrix()) {
    if(class(y)=="matrix"){
      x <<- y
      m <<- NULL
    }
    else
    {
      print("Input is not matrix")
    }
  }
  
  get <- function() x
  
  ##Set the Inverse
  setInverse <- function(Inverse = matrix())
  {
    ## Checks if the inverse is a matrix. So, any other types will be ignored
    if(class(Inverse)!="matrix")
    {
      print("Input is not matrix")
    }
    else
    {
      m <<- Inverse
    }
  }
  
  ##return the inverse stored in the method.
  getInverse <- function() m
  
  ##The list containg the functions to access the matrix and its inverse.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will return the inverse of the matrix stored in the vector,
## created by makeCacheMatrix function. If the inverse is not already stored in the vectory,
## then this function will compute Inverse, store it n the vector and return the inverse. 

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
