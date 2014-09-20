## To make matrix inversion more efficient, these functions cache previously inverted 
## matrices.

## makeCacheMatrix creates an object-like matrix, storing orgMatrix as the original matrix,
## and is able to store an inverted version of it, in itself.

makeCacheMatrix <- function(orgMatrix = matrix()) {
  ## Set the initial state for the inverted matrix
  invMatrix <- NULL
  ## Set stores a new matrix, and resets the inverted matrix to the initial state
  set <- function(newMatrix) {
    orgMatrix <<- newMatrix
    invMatrix <<- NULL
  }
  ## Get returns the original matrix
  get <- function() 
    orgMatrix
  ## setInv stores the inverted matrix, given as a parameter
  setInv <- function(newInvMatrix) 
    invMatrix <<- newInvMatrix
  ## getInv return the inverted matrix. Returns NULL when not set.
  getInv <- function() 
    invMatrix
  ## functions are accesible through a list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) 
}
## cacheSolve returns a matrix that is the inverse of the original matrix stored in x. 
## Before calculating the inverse of x, it checks whether it has been calculated before, and retrieves 
## its inverse value from cache

cacheSolve <- function(x, ...) {
  ## Check if there is a stored inverse for x
  inv <- x$getInv()
  
  ## If so, return cached value
  if(!is.null(inv)) {
    message('Cached data:')
    return(inv)
  }
  ## No data was cached, calculate the inverse.
  message('Calculated data:')
  inv <- solve( x$get())
  ## Save the inverse into the Matrix object
  x$setInv(inv)
  ## Return the newly calculated inverse matrix
  inv
  }

