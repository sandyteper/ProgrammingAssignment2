makeCacheMatrix <- function(x = matrix()) {
	##This function creates a list that will be used by the function cacheSolve
	##to find the inverse of a square inversible matrix.  The list contains
	##the items 'set' (a function to set the value of the inverse), 'get' (stores
	##the input matrix), 'setmatrix' (a function to set the value of the inverse)
	##and 'getinverse' (the inverse itself).
	##
	##The input matrix is assigned to x.

	m <- NULL
	set <- function(y = matrix()) { 
		x <<- y
		m <<- NULL
	}
	
	get <- function() x  ##Stores original matrix
	setmatrix <- function(inverse) m <<- inverse
	getinverse <- function() m  ##Stores matrix inverse 
	list(set = set, get = get,
		setmatrix = setmatrix,
		getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
      ##This function calculates and caches the inverser of a square
	##inversible matrix.  It accepts as input a list containing the 
	##the following items:
	##	1) 'set' (a function to set the value of the inverse)
	##    2) 'get' (stores the input matrix)
	##    3) 'setmatrix' (a function to set the value of the inverse)
	##	4) 'getinverse' (the inverse of the input matrix)
	##
	##The input matrix is assigned to x.

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setmatrix(m)
	m

}

