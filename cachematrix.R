#####################################################
#                       
#
#         Cache Inverse of Matrix
#
####################################################


#
# This function creates a special matrix that is able to cache its inverse by 
# storing a list of functions: set, get, setinverse, getinverse.
# These functions can be called individually by name_of_special_matrix + $ + 
# name_of_function.

makeCacheMatrix <- function(x = numeric()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inmatrix) im <<- inmatrix
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




#
#You can use this matrix to run as argument to makeCacheMatrix():
# samp <- sample(100,16)
# mat <- matrix(samp, 4,4)
# 


#
# This function checks if the special matrix created by the above function
# has a cached inverse. If yes it returns a message stating that it does and 
# and the inverted matrix.
# If not it computes the inverse, caches it in the special matrix, and returns it.
#
#

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
