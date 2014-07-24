## These functions attempt to solve the inverse of a Matrix, using the cached
## solution if the inverse of the Matrix has already been solved 

## This function takes in a Matrix and
## and returns the memory addresses for 
## various functions that set and get the data
## and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL ##initialize m 

	
	set <- function(y) 
	{
		x <<- y
		m <<- NULL ##if the data is being set, reset 'm'
	}

	get <- function() x 

	setInverse <- function(solve) m <<- solve
	getInverse <- function() m

	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function takes in the pointers to the data,
## the inverse of the unchanged data, and the get/set
## inverse functions and checks if there has been 
## a change to 'm'.  If not, calculate the inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	 
	m <- x$getInverse()
	
	if(!is.null(m)) ##check if the inverse has been calculated
	{
		message("getting cached data")
		return(m)
	}

	##else calculate the inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
