## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly
## Below is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##Initialize inverse 
        inv <- NULL
        ##Set the Matrix x
        set <- function(mat) {
                x <- mat
                inv <- NULL
        }
        ##Get the Matrix x
        get <- function() x
        ##Set the Inverse of Matrix x
        setInverse <- function(solve) inv <<- solve
        ##Get the Inverse of Matrix x
        getInverse <- function() inv
        ##create list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinverse()
        if (is.null(invM)){
                data <- x$get()
                invM <- solve(data, ...)
                x$setInverse(invM)
        }
        invM
}
