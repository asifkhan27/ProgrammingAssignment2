
makeCacheMatrix <- function(x = matrix()) {     ## Store matrix in cache

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {                   ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)){                                 ##Check matrix is already compiled or not
    message("getting cached data")
    return(m)
  }
  matrix1 <- x$get()
  m <- solve(matrix1,...)
  x$setInverse(m)                                 ##Print inverse matrix
  m
}
