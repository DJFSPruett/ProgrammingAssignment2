## The following two functions 1) inverts a matrix and caches the result
## then 2) allows retrieved results to be applied to other matrices

## Inversion function with INV as the applied effect/cached function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <-function() x
  setinverse <-function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Retrieves, applies to the selected or loaded data, returns result

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached")
    retun(inv)
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setinverse(inv)
  inv
}
