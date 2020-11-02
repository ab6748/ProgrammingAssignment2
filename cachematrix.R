## These two functions are used to compute the inverse of a given matrix and 
## store it in cache to be retrieved when needed without doing the whole computation again.

## The first function generates an environment for the objects required to get and set,
## the base matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- matrix()
  set <- function(y) {
    x <<- y
    s <<- matrix()
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function searches if the inverse matrix was computed previously. If not,
## then it generates the inverse matrix and stores it on the cache. Finally, it prints out
## the inverse matrix.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.na(s[1])) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
