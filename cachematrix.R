
##
## The following functions, makeCaheMatrix and cachSolveMatrix 
## are used to create and retrieve a matrix and inverse matrix.
## These functions reduce the costly computation by caching the 
## inverse of a matrix rather than computing it repeatedly.
## 


## This function creates a special "matrix" object that can cache 
## its inverse.
## This functions has 4 methods:
##      set                     - the value of the matrix
##      get                     - the value of the matrix
##      setInverseMatrix        - the value of the Inverse Matrix
##      getInverseMatrix        - the value of the Inverse Matrix
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(matr) m <<- matr
        getMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getMatrix = getMatrix)

}

##
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated, 
## then cacheSolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
