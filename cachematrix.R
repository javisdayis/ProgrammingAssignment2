## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function keep our before inverse matrix and ask for it
## before doing any computation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
    m <- x$getinverse()           #query the x vector's cache         
    if(!is.null(m)) {           #if there is a cache
      message("getting cached data") 
      return(m)                #just return the cache, no computation needed
    }
    data <- x$get()             #if there's no cache
    m <- ginv(data, ...)        #we actually compute them here
    x$setinverse(m)                #save the result back to x's cache
    m                           #return the result
  
}
