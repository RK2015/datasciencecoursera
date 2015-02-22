## Calculating Inverse some times very expensive operation. 
## So here this function will take matrix as input and then calculate the Inverse. 
## Since this is very time and resource consuming operation the second function will cache first time and then use from the cache.


## This function will take matrix as input and calculate Inverse of that matrix
## Thru this function we can set the matrix value, get the matrix value,
## and also set matrix Inverse and get matrix Inverse as well.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function will keep the Inverse value in cache is its already not in the cache. 
## If it is the cache then will not cache and return value from cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
	data <- x$get()
        m <- solve(data, ...)
        x$getsolve(m)
        m
}
