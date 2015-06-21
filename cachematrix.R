## The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        new.matrix = x$get()
        inv = solve(new.matrix, ...)
        
        # sets the value of the inverse in the cache
        x$setinv(inv)
        
        return(inv)
}
