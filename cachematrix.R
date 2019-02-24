## These two functions serve to store an invertible matrix and
## cache its inverse

## This first function holds a list of functions to store and
## access both the invertible matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function calculates the inverse of the matrix
## makeCacheMatrix returns and caches it into the list reserved
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv_m <- x$getinv()
	if(!is.null(inv_m)) {
		message("getting cached data")
		return(inv_m)
	}
	data <- x$get()
	inv_m <- solve(data, ...)
	x$setinv(inv_m)
	inv_m
}
