## Coursera : Data Science : R-Programming 
## Week 3 Assignment : Caching the Inverse of a Matrix
## Github : github.com/guamie

## Function : makeCacheMatrix
## The function creates a matrix object to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	
	cInverse <- NULL	## Initialize cInverse to store inverse matrix 

	set <— function(y) {	## Define function to assign new matrix value in parent environment
		x <<- y
		cInverse <— NULL
	}

	get <- function() x    ## Define Function to return matrix value

	setInverse <- function(inverseValue) cInverse <<- inverseValue		## Set inverse value in parent environment
	getInverse <- function cInverse               				## Get inverse value in parent environment
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function : cacheSolve
## Computes inverse of matrix using makeCacheMatrix

cacheSolve <- function(x, ...) {
	
	cInverse <- x$getinverse()

	if(!is.null(cInverse)) {			## Checks to see if cInverse is not NULL			
		return(cInverse)
	}

	data <- x$get()
	cInverse <- solve(data)
	x$setinverse(cInverse)
	cInverse
}
