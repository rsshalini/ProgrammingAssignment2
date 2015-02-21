## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix takes matrix as an argument.If no argument is given, then makeCacheMatrix will create a empty numeric matrix
makeCacheMatrix <- function(x = numeric()) {  
	i <- NULL 
## Function starts with setting inverse to NULL as a placeholder for a future value
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
## defines a function to set the matrix x to a matrix y and resets the inverse, i to NULL.
	get <- function() x 
## returns the matrix, x.
	setinverse <- function(inverse)  i <<- inverse
## sets the inverse, i, to inverse
	getinverse <- function() i
## returns the inverse, i
	list (set = set, get= get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}
## returns the 'special matrix' containing all of the functions just defined. 


## this function calculates the inverse of the matrix and returns. If the inverse is already caculated, the inverse of x is stored in variable i. 
## It retrieves the inverse from the variable i and prints the messgage that it is getting cached data and then the value of i.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i){
	       message("getting cached data")
	       return(i)
		   } 
## if the inverse is calculated already, variable i will not be emplty. Hence, the message is printed and then the value of the inverse of the matrix is retrieved.
		   data <- x$get()
		   i <- solve(data, ...)  
## inverse is calculated in this function and returned for the first time. 
	   x$setinverse(i) 
	   i
}
