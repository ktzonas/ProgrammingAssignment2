## Description: Functions for caching the inverse of matrices.
## R Programming Course, Assignment 2


# Create and return a 'matrix cache', which holds the given matrix 'matr',
# as well as its inverse.
# 
# The cache API is implemented as a list containing the following functions:
#   getmatrix(): Returns the wrapped matrix
#   setmatrix(matr.new): Replaces the wrapped matrix with 'matr.new' and clears the cached inverse
#   setinverse(inverse.new): Replaces the cached inverse with 'inverse.new'
#   getinverse(): Returns the cached inverse. If no inverse has been cached, NULL is returned.
makeCacheMatrix <- function(matr = matrix()) {
    inverse <- NULL
    
    setmatrix <- function(matr.new) {
        matr <<- matr.new
        inverse <<- NULL
    }
    
    getmatrix <- function() {
        matr
    }
    
    setinverse <- function(inverse.new) {
        inverse <<- inverse.new
    }
    
    getinverse <- function() {
        inverse
    }
    
    list(getmatrix = getmatrix, setmatrix = setmatrix, 
         getinverse = getinverse, setinverse = setinverse)
}


# Return the inverse of the given matrix.
# 
# This function accepts 'matrix caches', created using 'makeCacheMatrix'.
# If the inverse is cached it is returned; otherwise, the function computes it
# and then caches it. 
cacheSolve <- function(cache, ...) {
    inverse <- cache$getinverse()
    
    if (!is.null(inverse)) {
        message('getting cached data')
    } else {
        # get the wrapped matrix, compute its inverse and cache it
        matr <- cache$getmatrix()
        inv <- solve(matr)
        inverse <- cache$setinverse(inv)
    }
    
    inverse        
}
