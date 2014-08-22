## cachematrix is a library in which matrix can be cached along with its inverse.

## makeCacheMatrix function takes in a matrix and caches it
## also its inverse after thats been set.

makeCacheMatrix <- function(x = matrix()) {
        
        # Inverse of a matrix
        i <- NULL
        set <- function(y) {
                x <<- y
                ## Reset inverse as the matrix is changed
                i <<- NULL
        }
        get <- function() x
        getinverse <- function() i
        setinverse <- function(inverse) i <<- inverse
        
        ## returns a list with methods to operate on matrix.
        list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)

}


## cacheSolve reads data from cache if exist.
## If cache is empty computes inverse and returns the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("returning inverse from cache")
                return(i)
        }
        data <- x$get()
        ## compute inverse
        i <- solve(data)
        ## cache inverse
        x$setinverse(i)
        i
}
