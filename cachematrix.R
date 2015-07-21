## To calculate and cache matrix inversion to save time 


## This function creates a list containing functions to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the solved matrix
#4.  get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() {x}
  setSolvedMatrix <- function(solved_matrix) {m <<- solved_matrix}
  getSolvedMatrix<- function() {m}
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setSolvedMatrix = setSolvedMatrix,
       getSolvedMatrix = getSolvedMatrix)
  

}


## This function computes the inverse of the matrix
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated, then should retrieve the 
## inverse from the cache.

## NOTE: This function asumes that the matrix returnd by 
## 'makeCacheMatrix' is always inversable

cacheSolve <- function(x, ...) {
  m <- x$getSolvedMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("Solving matrix")
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setSolvedMatrix(m)
  m
}
