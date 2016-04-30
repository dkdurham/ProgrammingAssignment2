## Provided that a matrix has an inverse, the first function
## will cache the matrix for later use and the second function 
## will produce the inverse of that matrix

## This function will cache a matrix as 'm'

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of matrix 'm'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat.data <- x$get()
        m <- solve(mat.data, ...)
        x$setinverse(m)
        return m
}
