## Put comments here that give an overall description of what your
## functions do
#  Make Cache of Matrix and store the cache to increase computation effectiveness
## Write a short comment describing this function
#  This function creates a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
	set <- function(y) {
		x <<- y
		invrs <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invrs <<- inverse
	getinverse <- function() invrs
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)	
}


## Write a short comment describing this function
#  This function stored the cache and return the matrix computed above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()
	  if(!is.null(invrs)) {
	  	message("getting cached data")
        	return(invrs)
	}
 	mat <- x$get()
 	invrs <- solve(mat, ...)
	x$setinverse(invrs)
	invrs
}
