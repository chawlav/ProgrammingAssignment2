## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	mat <- NULL
	set <- function(y){
		x <<- y
		mat <<- NULL
	}
	get <- function() x

	getInverse <- function() mat

	setInverse <- function(data) mat <<- data

	list(set = set, get = get,  setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

	inv<- x$getInverse()
	if (!is.null(inv)){
		message("cached inverse")
		return(inv)
	}
	# need to compute
	mat <- x$get()
	inv <- solve(mat)
	x$setInverse(inv)
	inv
       
}
