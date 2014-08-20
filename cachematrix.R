## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
	# Define the cache variable
	mat_inv <- NULL
	
	# Function that sets the matrix
	setMatrix <- function(new_mat) {
		# Set the matrix and reset its inverse
		mat <<- new_mat
		mat_inv <<- NULL

		# Return something useful
		return (mat)
	}

	# Function that retrives the matrix
	getMatrix <- function() {
		return (mat)
	}

	# Function that sets the inverse
	setMatrixInverse <- function(inverse) {
		mat_inv <<- inverse

		# Return something useful
		return (mat_inv)
	}

	# Function that retrieves the inverse
	getMatrixInverse <- function() {
		return (mat_inv)
	}

	# Return the generated object
	cache_matrix <- list(setMatrix = setMatrix,
			     getMatrix = getMatrix,
			     setMatrixInverse = setMatrixInverse,
			     getMatrixInverse = getMatrixInverse)
	return (cache_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(cache_matrix, ...) {
	## Return a matrix that is the inverse of 'cache_matrix'

	# Look for cached data
	inv <- cache_matrix$getMatrixInverse()

	# Check if it is valid
	if (!is.null(inv)) {
		# It is a valid cache
		message("getting cached data")
		return (inv)
	}

	# No valid cache found
	# We need to generate it

	# Get the matrix from the cacheMatrix object
	mat <- cache_matrix$getMatrix()

	# Calculate its inverse
	inv <- solve(mat, ...)

	# Set the cache
	cache_matrix$setMatrixInverse(inv)

	# Return the inverse
	return (inv)
}

