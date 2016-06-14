## makeCacheMatrix creates a matrix that can store its inverse 
## cacheSolve retrieves that inverse if it exists, otherwise calculates the inverse, and sets it on the special matrix

## this function takes a inversible matrix as an argument, and returns a list with
## functions for getting the original matrix, getting the inverse of the matrix,
## and setting the inverse   
makeCacheMatrix <- function(x = matrix()) {
  
    x <- NULL
    
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(value) inverse <<- value
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function takes a makeCacheMatrix(matrix) and retrieves the inverse if its set
## if it isn't set, the inverse on the makeCacheMatrix(matrix) is set and
## finally, the inverse matrix is returned.

cacheSolve <- function(x) {
   inverse <- x$getinverse()
   if(!is.null(inverse)) {
     message("getting cached inverse data")
     return(inverse)
   }
   
   data <- x$get()
   inverse <- solve(data)
   x$setinverse(inverse)
   inverse
}

