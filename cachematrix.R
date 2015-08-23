# Data Science Specialization
# Course 2: R Programming
# Assigment 2: Caching the Inverse of a Matrix
# Fausto Martín López

# In this file two functions are declared to allow "caching" the inverse of a matrix, 
# as an example of caching to save computation time and increasing performance.


## This function creates a special "matrix", that with getters and setters
## allows simulating the caching of the inverse of the matrix along with
## the matrix itself.
makeCacheMatrix <- function(x = matrix()) {
    
    # Inverse of the matrix
    inv <- NULL
    
    # Setter for the matrix
    set <- function(y) {
        # Assigns the matrix
        x <<- y
        # The inverse is initialized to NULL
        inv <<- NULL
    }
    
    # Getter for the matrix
    get <- function() x
    
    # Setter for the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Getter for the inverse of the matrix
    getInverse <- function() inv
    
    # list for the special "matrix"
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This funcion returns the inverse of a matrix. 
## First it checks if the inverse has already been calculated and cached, 
## and if so, returns the cached value.
## Otherwise it calculates the inverse, caches it and then returns it.
cacheSolve <- function(x, ...) {
    
    # Get the inverse from the special "matrix"
    inv <- x$getInverse()
    
    # If it's not null return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If it's null then calculate, cache and return the inverse
    data <- x$get()
    ## solve() function calculates the inverse of a matrix in R
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
