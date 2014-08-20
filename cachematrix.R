
## We create a pair of functions that cache the inverse of a matrix.  This 
## can save computing time by not having to repeatedly recompute the 
## inverse of a matrix.

## The first function `makeCacheMatrix` creates a special "matrix" object
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}

## The second function `cacheSolve` computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.
## We  assume that the matrix supplied is always invertible.

## To run these functions, use these commands, for example:
##   Mat<- matrix(1:4, 2, 2)
##   matobject <- makeCacheMatrix(Mat)
##   cacheSolve(Mat)


cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}







