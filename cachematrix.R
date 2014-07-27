## This function returns a list of functions which are able to cache the value for a matrix, return the value of that matrix, cache the value of the inverse of that matrix and return the value of that inverse.
makeCacheMatrix <- function(x = matrix()) {

	## Sets the InvMat variable which will later store the inverse of the matrix to NULL
	InvMat <- NULL
	
	## Caches the matrix as variable x and sets the inverse of that matrix to NULL
	set <- function(y) {
		x <<- y
		InvMat <<- NULL
	}
	
	## Returns the required original matrix
	get <- function() x

	## Stores the inverse of the matrix as InvMat
	setSolveMatrix <- function(InverseMatrix) InvMat <<- InverseMatrix

	## Returns the inverse of the matrix
	getSolveMatrix <- function() InvMat

	## Returns the newly defined functions and their individual returned values including the original matrix and its inverse
	list(set = set, get = get, setSolveMatrix = setSolveMatrix, getSolveMatrix = getSolveMatrix)

}


## This function caculates the inverse of a matrix. It checks whether an inverse of that matrix first exists. If yes, it loads the already existing one. Else, it caculates the inverse and caches the data for future reference.
cacheSolve <- function(x, ...) {
        
	## Stores the existing cached inverse as InvMat
	InvMat <- x$getSolveMatrix()
	
	## Checks whether the InvMat variable exists or is empty
	if(!is.null(InvMat)) {

		## In the case that it is not empty, a message is shown that the cached data is being loaded.
		message("getting cached data")

		## The cached inverse matrix is returned
		return(InvMat)
	}

	## Takes the required matrix from the get() function above
	data <- x$get()
	
	## Calculates the inverse of the matrix above and stores it in InvMat
	InvMat <- solve(data, ...)

	## Caches the inverse denoted by InvMat
	x$setSolveMatrix(InvMat)

	## Return a matrix that is the inverse of 'x'
	InvMat
}