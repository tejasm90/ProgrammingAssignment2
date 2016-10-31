## This function creates a special "matrix" object that can cache its inverse
## It establishes an environment where the target matrix and its inverse
## are defined and preserved.
## Then it returns a list, where each element of the list is a function
## which either 1) Sets the value of the matrix; 2) Gets the matrix; 
## 3) Sets the value of the inverse of the matrix; 4) Gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function described above.
## If the inverse has already been calculated, then it should return
## the inverse from the cache.
## using the solve() function to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
