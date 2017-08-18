## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                                    
    set <- function(y) {
        x <<- y # Update the old matrix to the new matrix
        inverse <<- NULL # Reset the inverse of the matrix of the new matrix
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve # Set the value of the inverse of the matrix
    getinverse <- function() inverse # Get the inverse of the matrix
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse() 

    # If the inverse of the matrix has been calculated 
    if(!is.null(inverse)) {    
        message("getting cached data")      
        return(inverse)                       
    }

    # If the inverse of the matrix has not been calculated
    data <- x$get()                              
    inverse <- solve(data, ...)                   
    x$setinverse(inverse)
    inverse
}
