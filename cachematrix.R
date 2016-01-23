## There are 2 functions, makeCacheMatrix and cacheSolve that setup the 
## list to store the inverse of e matrix

## Setup the list to store the matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invmatrix) invm <<- invmatrix
        getinvmatrix <- function() invm
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## Perform the inverse and store it if not already present and return the inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinvmatrix()
		if(!is.null(m)) {
		message("getting cached matrix data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinvmatrix(m)
		m
}
