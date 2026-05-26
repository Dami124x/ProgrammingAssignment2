## These functions create a special object that stores a numeric matrix
## and caches its inverse to avoid computing it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Initialize the inverse property to NULL
        
        # Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL # Reset the inverse if a new matrix is set
        }
        
        # Method to get the matrix
        get <- function() x
        
        # Method to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        
        # Method to get the inverse of the matrix
        getInverse <- function() inv
        
        # Return a list of the methods
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # Just return the inverse if it is already set
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Get the matrix from our object
        data <- x$get()
        
        # Calculate the inverse using the solve() function
        inv <- solve(data, ...)
        
        # Set the inverse to the object
        x$setInverse(inv)
        
        # Return the matrix
        inv
}
