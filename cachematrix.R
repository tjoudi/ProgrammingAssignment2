## To calculate and cache matrix inversion to save time 


## This function creates a list containing functions to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the solved matrix
#4.  get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  # empty cache
  m <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    # empty cache
    m <<- NULL
  }
  
  getMatrix <- function() {x}
  
  setSolvedMatrix <- function(solved_matrix) {m <<- solved_matrix}
  
  getSolvedMatrix<- function() {m}
  
  # create the list that contains the setters and the getters
  # for both original matrix and the cached solved matrix
  list(
      setMatrix = setMatrix
       , getMatrix = getMatrix
       , setSolvedMatrix = setSolvedMatrix
       , getSolvedMatrix = getSolvedMatrix
      )
  

}


## This function computes the inverse of the matrix
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated, then should retrieve the 
## inverse from the cache.

## NOTE: This function asumes that the matrix returnd by 
## 'makeCacheMatrix' is always inversable

cacheSolve <- function(x, ...) {
  
  solved_m <- x$getSolvedMatrix()
  
  if(!is.null(solved_m)) {
    message("getting cached data")
    return(solved_m)
  }
  
  message("Solving matrix")
  original_m <- x$getMatrix()
  solved_m <- solve(original_m, ...)
  
  # cache the solved matrix
  x$setSolvedMatrix(solved_m)
  # return the solved matrix
  solved_m
}
