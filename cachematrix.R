## These functions cache the value of the inverse of a matrix, and if the matrix
## that is passed to cacheSolve is already cached (i.e is the same data passed
## again), then returns the cached value, otherwise it is calculate the inverse
## of the matrix and this value will be stored through the functions defined, 
## in the function makeCacheMatrix, and stored in x that will be returned.


## This function creates a special object x, that contains four functions: 
## setMatrix: set the value of the x
## getMatrix: return the value of the x
## setInverse: set the inverse of the x
## getInverse: get the inverse of the x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(value) {
        x <<- value
        m <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverse <- function(inverseValue) m <<- inverseValue
    getInverse <- function() m
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         getInverse = getInverse, setInverse = setInverse)
}


## This function returns a cached value for the x matrix or calculate the inverse
## of the x and then cache and return the value.
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
            message("Getting the cached data.")
            return(m)
        } else {
            matrix <- x$getMatrix()
            
            m <- solve(matrix, ...)
            x$setInverse(m)
            m
        }
}
