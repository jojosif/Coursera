## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function is to create a special type of matrix, a cacheable matrix.
# It takes a square matrix as an arguement and generates a list of operations: 
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix
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

# cacheSolve function calculates the inverse of the special "matrix" created with the above makeCacheMatrix function. 
# Firstly, it checks if the inverse of the matrix has already been calculated. 
# If so, it gets the inverse of the matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix of the data and sets the value 
# of the inverse of the matrix in the cache using the setinverse function.
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
