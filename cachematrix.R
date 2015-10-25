## Programming Assignment No. 2: The following two functions are used: (a) to create a special
## special matrix object that can cache its inverse, and (b) compute the inverse of the same special
## inverse after first determining whether or not the inverse has been calculated. Assigment
## gives assumptions that input is always a square matrix that is invertible so no inital  
## inital testing of the input will be done.

## Create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
  i <- NULL                                ## initialize inverse to be NULL
  set <- function(y) {                     ## set up some initial values
    x <<- y                                ## cache matrix input
    i <<- NULL                             ## cache inverted matrix
  }
  get <- function () x                     ## Get the matrix
  setInverse <- function (i) i <<- solve(x) ## Set the inverse of the matrix
  getInverse <- function () i               ## Retrieve the inverse of the matix
  ## The four functions are contained in a list.
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Calculate the inverse of the special matrix which was created in the makeCacheMatrix
## function. Initially 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()                       ## Has inverse already been calculated?
  if(!is.null(i)) {                         ## Yes - return inverse
    return(i)
  }
  i <- solve(x$get())                       ## No - calculate inverse 
  x$setInverse(i)                           ## Cache the inverse
  i                                         ## Returninverse of the matrix
}
