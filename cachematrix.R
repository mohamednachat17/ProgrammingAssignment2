
# Function to create a special matrix object with caching
makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    
    set <- function(newValue) {
        mat <<- newValue
        inverse <<- NULL
    }
    
    get <- function() mat
    
    setInverse <- function(newInverse) inverse <<- newInverse
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the matrix using caching
cacheSolve <- function(cacheMatrix, ...) {
    # Return the cached inverse if available
    if (!is.null(cacheMatrix$getInverse())) {
        message("Getting cached data")
        return(cacheMatrix$getInverse())
    }
    
    # Compute the inverse using solve
    mat <- cacheMatrix$get()
    inverse <- solve(mat, ...)
    
    # Cache the computed inverse
    cacheMatrix$setInverse(inverse)
    
    inverse
}

# Example usage:
# Create a cache matrix
myMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute the inverse using cacheSolve
cacheSolve(myMatrix)


