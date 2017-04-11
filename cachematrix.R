## This assignment aims to create a function to calculate the inverse 
## of a matrix using scoping rules

## The function makeCachMatrix creates the environment variables

makeCacheMatrix <- function(x = matrix()) {

    #First of all, inicialize the variable inv, for cached inverse
    inv <- NULL
  
    #Than set the matrix
    set <- function( m ) {
      x <<- m
      inv <<- NULL
    }
    
    # Get Matrix
    get <- function() x #Return the matrix
    
    # Set the Inverse Matrix
    setInverse <- function(inverse){
      inv <<- inverse
    }
    
    # Get the Inverse Matrix
    getInverse <- function(){
      inv
    }
    
    #Set a list of chars containing the functions
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Function that uses makeCacheMatrix to calculate the Matrix Inversed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## Return the inverse of the Matrix, if it is set
    if( !is.null(inv) ) {
      message("getting cached data")
      return(inv)
    }
    
    ## Get the matrix from stored data
    data <- x$get()
    
    ## Calculate the inverse
    inv <- solve(data,...)
    
    ## Set the matrix inverse
    x$setInverse(inv)
    inv
}
