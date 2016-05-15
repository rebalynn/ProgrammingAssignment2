## cachematrix is to save time when computing the inverse
## of a matrix multiple times, perhaps in a loop

## This function creates a special "matrix" object that can cache its inverse

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


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

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
