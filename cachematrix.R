## First we make a special matrix with makeCacheMatrix to be able to cache the inverse.
## With function cacheSolve we return the cache of inverse if it has already been solved.

## With this function we can set and get the value of the matrix and the inverse.

makeCacheMatrix <- function(x) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## With this function we calculate the inverse of the matrix, except if it has already been calculated.

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
