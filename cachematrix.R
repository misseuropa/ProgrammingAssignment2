## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It is better to calculate it only once, and get it later.
## Than to redo the same work over and over. 


makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(solve) m <<- solve
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## cacheSolve either gets and returns the solved matrix if it is in the cache
## or if not, it gets the matrix, solves it and returns it.

cacheSolve <- function(x, ...) {
  
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}
