## These functions mirror the functionality of 'makeVector' and 'cachemean' example functions
## from the assignment description


## Creates a 'special' matrix

makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL
	get <- function(){ x }
	set <- function(new_x){
		x <<- new_x
		cache <<- NULL
	}
	getSolve <- function(){ cache }
	setSolve <- function(s){ cache <<- s }
	list(get = get, set = set, getSolve = getSolve, setSolve = setSolve)
}


## Calculates the inverse of the matrix, stores the result in the special matrix for future use
## then returns the result (the inverse).

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getSolve()
	if(!is.null(inv)){
		# optionally message that you return the cache
		# message("inverse returned from cache")
		return(inv)
	}

	inv <- solve(x$get(), ...)
	x$setSolve(inv)
	inv
}

