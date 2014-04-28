## Matrix inversion is usually a costly computation 
## and their may be some benefit to cachinng 
## the inverse of a matrix rather than compute it
## repeatedly. The following pair of functions caching 
## the inverse of a matrix.
## 

## the first function makeCacheMatrix creates a special
## "matrix", which is really a list containing a func to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_m <<- inverse
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## the following function calculates the inverse of 
## the special "Matrix" created with the above function.
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the
## value of the inverse in the cache via func setinverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
