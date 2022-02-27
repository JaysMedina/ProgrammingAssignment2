#### makeCacheMatrix ####
# this function takes a matrix x and caches its inverse and returns a list of its functions
# inverse inside is the variable name for the matrix inverse
# at the start of the function, the inverse variable is cleared, essentially clearing the cache
makeCacheMatrix <- function(x = matrix()) {
  # Cache inverse of given matrix
  inverse <- NULL
  
  # define getter and setter functions for the matrix
  getMatrix <- function() x
  setMatrix <- function(temp){
    x <<- temp
    inverse <<- NULL
  }
  
  # define getter and setter functions for the inverse matrix
  getInverse <- function() inverse
  setInverse <- function(temp) inverse <<- temp
  
  # return list of children functions
  list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


#### cacheSolve ####
# this function computes for the matrix inverse
# however, if the function sees that the calculation has already been done (directly before), it will simply
# retrieve the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # return cached matrix inverse if it exists already
  if (!is.null(inverse)){
    message("fetching inverse matrix")
    return(inverse)
  }
  
  # compute for matrix inverse
  temp <- x$getMatrix()
  inverse <- solve(temp,...)

  # cache the computed matrix inverse
  x$setInverse(inverse)
  
  # return matrix inverse
  return(inverse)
}

#### Test ####
test1 <- matrix(sample(1:50,9),nrow=3, ncol=3, byrow=TRUE)
test1cache <- makeCacheMatrix(test1)
cacheSolve(test1cache)
cacheSolve(test1cache) # done to show that the cache works
