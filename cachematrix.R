## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix takes an input matrix x and returns a list 
#containing the following functions:
#
# set:  sets the matrix to the desired value
# get:  gets the value of x
# setinverse: stores the inverse of x in m via solve()
# getinverse: gets the value of m

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
#
# cacheSolve takes an input matrix x and will return one 
# of the following:
# 1.  The inverse of x from the cache if the inverse has previously 
#     been solved
# 2.  The inverse of x from the R function solve() if the cache is empty
#     (i.e. if m <- NULL)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
