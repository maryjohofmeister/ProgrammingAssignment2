##R programming assignment 2
##The following script has two functions: makeCacheMatrix and cacheSolve. See descriptions below

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets the value of m to NULL 
  set <- function(y) { ## creates function set that assigns matrix y to x
    x <<- y
    m <<- NULL ## sets the inverse of the matrix to null
  }
  get <- function() x ##creates a get function that gets x from cache
  setmatrix <- function(matrix) m <<- matrix ##creates a setmatrix function, assigns matrix to m in cache
  getmatrix <- function() m ##creates a getmatrix function, calls m from cache
  list(set = set, get = get, ##prints out list with created functions: set, get, setmatrix, getmatrix 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix() ##assigns matrix x to m
  if(!is.null(m)) { ##if m already exists then..
    message("getting cached data") ##print message
    return(m) ## and return cached m
  }
  data <- x$get() ## else assign matrix to object data
  m <- solve(data, ...) ##solve matrix for the inverse
  x$setmatrix(m) ##cache inversed matrix
  m ##print m
}
