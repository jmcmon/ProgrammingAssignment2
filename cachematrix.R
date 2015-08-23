
## Overall description
## makeCacheMatrix takes a matrix as its argument and sets up functions in a list to return the inverse of that matrix using the 'solve function'
## cacheSolve is a wrapper for solve() which returns the inverse of the matrix if already calculated, or works it out from scratch if not already calculated.

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to get and set the matrix argument and to get and set the inverse of the matrix, using solve()
## The <<- operator broadens the scope of 'm' so that the value of m can be checked from the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix parameter which is passed to it. Lexical scoping of 'm' means that 'm' can be checked inside this function
## to see if it is null. If null, the inverse must be calculated from scratch. If not null, the inverse has already been calculated and assigned to
## 'm' and does not need to be re-calculated. The value of 'm' is returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}