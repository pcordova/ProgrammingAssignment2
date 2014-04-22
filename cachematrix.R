## The execution environment normally disappears after the function returns a value.
## Because every time a function is called, a new environment is created to host execution,
## a function has no way to tell what happened the last time it was run;
## each invocation is completely independent (a fresh start).
## However, functions capture their enclosing environments.
## This means when function a returns function b, function b captures and stores
## the execution environment of function a, and it doesn’t disappear.
## (Hadley Wickham, "Advanced R")

## The above text gives us a clue about how to cache objects
## in case we need future access to them.

## The function "makeCacheMatrix" will create a list containing "getters" and "setters",
## those four functions will allow "cacheSolve" to do its things. Furthermore,
## "makeCacheMatrix" initializes the original matrix (m1) and its inverse (inv).

makeCacheMatrix <- function(m1 = matrix()) {
    inv <- matrix()
    set <- function(m2) {
           m1 <<- m2
           inv <<- matrix()
    }
    get <- function() m1
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function "cacheSolve" is who returns a matrix that is the inverse of 'm1'
## This function computes the inverse of the special "matrix" returned by
##`makeCacheMatrix` above, using the methods contained in the returned list.

cacheSolve <- function(m1, ...) {
    inv <- m1$getinv()
    if(is.numeric(inv)) {              ## If the inverse has already been calculated
        message("getting cached data") ## (and the matrix has not changed), then
        return(inv)                    ## "cacheSolve" should retrieve the inverse from the cache.
    }
    data <- m1$get()
    inv <- solve(data, ...)            ## A matrix that is the inverse of 'm1' is created
    m1$setinv(inv)                     ## (in case the above "if" evaluates as "FALSE").
    inv
}
