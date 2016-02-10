## Put comments here that give an overall description of what your
## functions do
# A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
# this function create a special matrix, which is a list containing functions to
# (1) set the value of the matrix; (2) get the value of the matrix;
# (3) set the inverse of the matrix; (4) get the inverse the matrix.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inv <<- inv
	getinv <- function() inv
	list(set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)){
			message("getting cached data")
			return(inv)
		}
		dta <- x$get()
		inv <- solve(dta)
		x$setinv(inv)
		inv
}
