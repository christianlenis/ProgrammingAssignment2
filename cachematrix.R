## The following functions cache the inverse of a matrix  
## and avoid recalculating it when it has already been computed.

## This function creates a "matrix object" that stores a matrix 
## and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Variable to store the cached inverse
        
        # Function to update the matrix and reset the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to retrieve the original matrix
        get <- function() x
        
        # Function to store the inverse in the cache
        setinverse <- function(inverse) inv <<- inverse
        
        # Function to retrieve the cached inverse
        getinverse <- function() inv
        
        # Returns a list with all the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix stored in the object 
## created by makeCacheMatrix. If the inverse has already been computed 
## and the matrix has not changed, the cached value is retrieved 
## to avoid recomputation.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If the inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse is not cached, compute and store it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
