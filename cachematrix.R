#### makeCacheMatrix ####

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
cacheSolve(test1cache)
