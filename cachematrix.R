## Following functions helps in computing inverse of a matrix. 
## If inverse of a matrix has already been computed, computation is skipped
## and cached answer is returned

## Function : makeCacheMatrix
## This function returns a list of functions containing a function to
## 1.) set value of a matrix
## 2.) get value of a matrix
## 3.) set inverse of a matrix
## 4.) get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL	
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## Function : cacheSolve
## This function computes inverse of a matrix. If inverse of matrix 
## is already computed, inverse is returned from cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)){
			message("getting cached data")
			return(inv)
		}
		mat <- x$get()
		inv <- solve(mat)
		x$setinv(inv)
		inv
}
