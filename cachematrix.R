## Put comments here that give an overall description of what your
## functions do

## This function takes in a matrix as in input, and makes a list containing
## functions to:-
## 1) set  the values of Matrix
## 2) get the matrix
## 3) set the value of the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


## This function takes in the matrix created from the previous function and
## first checks if the inverse has already been calculated. If it has been,
## then it skips the calculation and directly shows the output. Otherwise,
## it calculates the inverse via the Solve() function and shows the output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  
  data <- x$getMatrix()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
  }
