makeCacheMatrix <- function(x = matrix()){
  ##set the value of the vector
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##get the value of the vector
  get <- function() x
 
  ##set the value of the inverse
   setinv <- function(solve) 
  m <<- solve
  ##get the value of the inverse
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(x, ...){
  ##Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

