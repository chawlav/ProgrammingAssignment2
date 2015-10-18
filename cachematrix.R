## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


	mat <- NULL
	
	#set function 
	set <- function(y){
		#set the value of x in the parent scope
		x <<- y
		#since the matrix data is changed when this function is called we must destroy the cached copy of the inverse
		mat <<- NULL
	}

	# return the matrix data
	get <- function() x

	# return the inverse (cached)
	getInverse <- function() mat

	#cache the inverse
	setInverse <- function(data) mat <<- data

	list(set = set, get = get,  setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

	# get the inverse using the supplied functionin the arguments (x)
	inv<- x$getInverse()
	#check not null
	if (!is.null(inv)){
		message("cached inverse")
		return(inv)
	}
	# inverse was not cached earlier hence needs to be computed
	# get the underlying data
	mat <- x$get()
	#assume matrix is invertible. compute the inverse
	inv <- solve(mat)
	#cache it
	x$setInverse(inv)
	inv
       
}
