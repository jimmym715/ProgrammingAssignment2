## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  solvedMatrix <- NULL
  set <- function(y) {
    x <<- y
    solvedMatrix <<- NULL
  }
  get <- function() x
  setSolvedMatrix <- function(sm) solvedMatrix <<- sm
  getSolvedMatrix <- function() solvedMatrix
  list(set = set, 
       get = get,
       setSolvedMatrix = setSolvedMatrix,
       getSolvedMatrix = getSolvedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  solvedMatrix <- x$getSolvedMatrix()
  if(!is.null(solvedMatrix)) {
    message("getting cached data")
    return(solvedMatrix)
  }
  data <- x$get()
  solvedMatrix <- solve(data)
  x$setSolvedMatrix(solvedMatrix)
  solvedMatrix
}
