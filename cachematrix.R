## Create an object containing a matrix and its cached inverse (if available)
makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix  <<- NULL  # Force reinitialization of inverse if matrix is changed
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){
    invMatrix <<- inverse
  }
  
  getinverse <- function(){
    invMatrix
  }
  
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Get the cached inverse if it exists already, otherwise calculates it
cacheSolve <- function(x,..) {
  
  # Get cached version
  invMatrix <- x$getinverse()
  
  # If cached version exists, return that value
  if(!is.null(invMatrix)) {
    message("getting cached inverse")
    return(invMatrix)
  }
  
  #Otherwise calculate it, insert it in cache and return its value
  tempMatrix <- x$get()
  
  message("calculating inverse")
  invMatrix <- solve(tempMatrix,..)
    x$setinverse(invMatrix)
  
  invMatrix
}
