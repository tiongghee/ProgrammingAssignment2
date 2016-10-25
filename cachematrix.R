## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes in a matrix x and returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## stores the inverse of the matrix
  
    ## stores the original matrix for comparison
    origm <<- x
    
    ## Use sapply to apply run solve to all the matrix values and return matrix.
    invm <<- as.matrix(sapply(x, solve))
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix
## cacheSolve will first check if there is an existing matrix, if not return inverse of x
## cacheSolve next check if the current matrix is identical to the previous, if yes, return the matrix from cache
## finally, if matrix is not identical, return the inverse of x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check if there is a cached matrix
  if (is.null(invm)) {
    ## Store current matrix before applying inverse
    origm <<- x
    invm <<- as.matrix(sapply(x, solve))
    return(invm)
  }
  
  ## check if the current matrix is the same as previous 
  if (identical(x, origm)) {
    return(invm)  
  }
  
  invm <<- as.matrix(sapply(x, solve))
}