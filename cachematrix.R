## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL # intialize the inverseMatrix as Null
  
  # create the setMatrix function
  setMatrix <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # create the getMatrix function
  getMatrix <- function() x
  
  # create the setInverseMatrix function
  setInverseMatrix <- function(inverse)
  {
    inverseMatrix <<- inverse
  }
  
  # create the getInverseMatrix function
  getInverseMatrix <- function() inverseMatrix
  
  # return the setters and getters
  return(list(setMatrix = setMatrix,
              getMatrix = getMatrix,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix))

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix)) # check inverseMatrix null or not
  {
    message("getting cached data")
    return(inverseMatrix)
  }
  else{
    matrixData <- x$getMatrix()
    # get the inverse of the matrix using solve function
    inverseMatrix <- solve(matrixData,...) 
    #set the inver matrix to setInverseMatrix
    x$setInverseMatrix(inverseMatrix)
    #return the inverse 
    return(inverseMatrix)
    
  }
  
}

# check the code is working correctly
square_matrix = matrix(c(1,2,3,4),2,2)
mcmatrix = makeCacheMatrix(square_matrix)
cacheSolve(mcmatrix)
cacheSolve(mcmatrix)
