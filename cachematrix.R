## Below are two functions that are used to create a special object
## that stores a numeric matrix and caches the inverse of the matrix

## The function makeCacheMatrix creates a 'matrix'.  The function actually
## is a list of functions which set the value of the matrix, get the value
## of the matrix, set the value of the inverse of the matrix, and get the
## value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
             set <- function(y) {
                         x <<- y
                         m <<- NULL
                     }
             get <- function() x
             setsolve <- function(solve) m <<- solve
             getsolve <- function() m
             list(set = set, get = get,
                             setsolve = setsolve,
                             getsolve = getsolve)
        
}


## The cacheSolve function calculates the value of the inverse of the matrix
## created above.  First it checks to see if the value has already been
## calculated.  If it has been, it gets the value of the inverse of the matrix
## from the cache and skips the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
             if(!is.null(m)) {
                         message("getting cached data")
                         return(m)
                     }
             data <- x$get()
             m <- solve(data, ...)
             x$setsolve(m)
             m
}
