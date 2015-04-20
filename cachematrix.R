## the combination of the two functions below allow one
## to calculate the inverse of an invertible square matrix
## no more than once, as the solution will be cached after 
## it's first determined, and subsequent requests for the solution
## will return the cached solution

## function expects an invertible square matrix as its sole parameter
## function stores provided matrix in local variable
## and provides its own functions to get/set the matrix
## along with getSolveMatrix and setSolvedMatrix functions

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


## function expects the results of makeCacheMatrix
## as its primary parameter
## if that parameter contains a non-null solvedMatrix
## then this function will return that solvedMatrix
## rather than re-solving
## if the parameter provided does not contain a non-null
## solvedMatrix, then this function will execute solve
## on the matrix of the provided parameter [(]as returned by x$get()]
## and pass the result of that operation to the 
## setSolvedMatrix function

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
