##  The following functions work in tandem to reduce computer
## 	work time in the memory costly computation of inverse matrices.
##	It accomplishes this by storing the inverse matri in the computer's
##  cache, where it can be retrieved later on.

## 	The first function creates a list of functions for getter/setter
##	access to the matrix and its inverse

makeCacheMatrix <- function(matrix = matrix()){
	inverseMatrix <- NULL
	setMatrix <- function(newMatrix){
		matrix <<- newMatrix
		inverseMatrix <- NULL
	}
	getMatrix <- function() matrix
	setInverse <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
	getInverse <- function() inverseMatrix
	list (setMatrix = setMatrix, getMatrix = getMatrix,
		  setInverse = setInverse, getInverse = getInverse)
}


##	The solve function returns the inverse of a specified special
##	matrix but only computes the inverse if it is not found in 
##	the cache.

cacheSolve <- function(m, ...){
	inverseMatrix <- m$getInverse()
	if(!is.null(inverseMatrix)){
			message("Retrieving the Inverse from the Cache")
			return(inverseMatrix)
	}
	data <- m$getMatrix()
	inverseMatrix <- solve(data, ...)
	m$setInverse(inverseMatrix)
	inverseMatrix
}