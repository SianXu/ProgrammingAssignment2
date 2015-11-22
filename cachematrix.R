## makeCacheMatrix is an object to store the input matrix x and its inversed matrix
## casheSolve calculates inversed matrix and store it in makeCacheMatrix

## two input: set and setMatrix; two output: get and getMatrix

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## get the input matrix in makeCacheMatrix
##if there's a inversed matrix storing there, return the matrix and exist function
##if not, calculate the inversed matrix and store it in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setMatrix(m)
  m  ## Return a matrix that is the inverse of 'x'
}

