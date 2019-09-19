## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverseMatrix <- function(inverse)
  {
    inverseMatrix <<- inverse
  }
  
  getInverseMatrix <- function() inverseMatrix
  
  return(list(setMatrix = setMatrix,
              getMatrix = getMatrix,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix))

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix))
  {
    message("getting cached data")
    return(inverseMatrix)
  }
  else{
    matrixData <- x$getMatrix()
    inverseMatrix <- solve(matrixData,...)
    x$setInverseMatrix(inverseMatrix)
    return(inverseMatrix)
    
  }
  
}

square_matrix = matrix(c(1,2,3,4),2,2)
mcmatrix = makeCacheMatrix(square_matrix)
cacheSolve(mcmatrix)
cacheSolve(mcmatrix)
