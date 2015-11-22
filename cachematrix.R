# Programming assignment 2: 
# Cache inverse of a matrix
# Calculate one if a cache is not found

# Caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function () x
  setsolution <- function(solve) solved <<- solve
  getsolution <- function() solved
  list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
}

# Solves a matrix. Returns a cached solution if one exists

cacheSolve <- function(x, ...) {
  solved <- x$getsolution()
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(x, ...)
  x$setsolution(solved)
  solved
}