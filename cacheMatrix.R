# Inversion of a matrix can be costly in time and memory computation and there could be a benefit
# to caching the inverse of a matrix rather than run a loop to compute it repeatedly. The
# following two functions can be used to utlize the cache  the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        dataset <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse2) inverse <<- inverse2
        getinverse <- function() inverse
        list(dataset=dataset, get=get, setinverse=setinverse, getinverse=getinverse)
        }    



# This function solves for the inverse of the matrix. It does an initial validation to see if
# the inverse has previously been calculated. If so, it retrieve the calculated result. 
# If the is no value, the funtion computes the inverse and sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        
}
