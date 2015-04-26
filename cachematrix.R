## The following functions help cache the invere of a matrix. 

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the mean
# 4. Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
      invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function returns the inverse of the matrix. Ir first checks to see if the  has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data)
        x$setinverse(invr)
        invr
}
