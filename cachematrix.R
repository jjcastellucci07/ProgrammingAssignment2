## Written by John Castellucci on 25 May 2014

## This pair of functions enable the user to make a special matrix that caches its own inverse so that it can be retrieved at will and only has to be computed once.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {

	 # Initializes the inverse
	inverse <- NULL

	 # Method to set the matrix (also clears the inverse)
	set <- function(B) {
		A <<- B
		inverse <<- NULL
	}

	 # Method to return the matrix
	 get <- function() A

	 # Method to set the inverse (does not solve!)
	setInverse <- function(solution) inverse <<- solution

	 # Method to return the inverse
	getInverse <- function() inverse

	 # Returns a list of methods
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(M, ...) {
	 # Retrieves the inverse from the cache
	inverse <- M$getInverse()

	 # Returns the cached inverse with a message, unless it is NULL
	if(!is.null(inverse)) {
		message("Getting cached data...")
		return(inverse)
	}

	 # Computes the inverse of matrix M
	inverse <- solve(M$get(), ...)

	 # Caches the matrix's inverse
	X$setInverse(inverse)

	 # Returns the inverse
	inverse
}