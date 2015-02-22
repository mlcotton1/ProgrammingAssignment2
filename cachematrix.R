## To cache (or save the result without reruning the program)
## Use of the '<<-' to prevent variables from going out of scope.
## We will create a matrix: makeCacheMatrix()
## CreateMatrix will contain four functions: set, get, setInv and getInv.
## We will solve for the inverse of the matrix: inverseSolve()

## makeCacheMatrix()
## By dfn of inverse we need to input a square, invertible matrix
## We need 4 items to solve for the inverse of the matrix
## Define the matrix: function set
## Retrieve the matrix: function get
## Define the inverse matrix: function setInv
## Retrieve the inverse matrix: function getInv

makeCacheMatrix <- function(x = matrix()) {
  
  # this is where the result of inversion is stored
  xinv <- NULL 
  
  #define the initial matrix
  set <- function(y) 
  {
    x <<- y
    xinv <<- NULL 
  }
  
  # retrieve the input matrix
  get <- function() x 
  # define the inversed matrix
  setInv <- function(inv) xinv <<- inv
  # retrieve the inversed matrix
  getInv <- function() xinv 
  
  # return a list that contains the four functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) 
{
  # get the inversed matrix from object x
  m <- x$getInv() 
  
  # if this is null, it will be calculated
  if(!is.null(m)) 
  { 
    message("Retrieving cached data")
    return(m) 
  }
  
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  return(m) 
}

