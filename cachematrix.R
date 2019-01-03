## Put comments here that give an overall description of what your
## functions do

## This function takes matrix object as a parameter and wraps it with list object which does cache management

makeCacheMatrix <- function(x = matrix()) {

	#
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function takes CacheMatrix object as prameter solves the matrix,
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
