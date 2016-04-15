## Contains two functions :
## makecachematrix : This function caches the matrix passed to it
## cachesolve : this function checks and creates inverse of original matrix
## 
## makecachematrix published methods  : 
## get : access the value
## set : assign the value
## getim : fetch the inverse matrix 
## setim : assign value of inverse matrix

makeCacheMatrix <- function(q = matrix()) {
    m <- NULL
    set <- function(y) {
        q <<- y
        m <<- NULL
    }
    get <- function() q
    setim <- function(im) m <<- im
    getim <- function() m
    list(set = set, get = get, setim = setim, getim = getim)
}


cacheSolve <- function(x, ...) {
    m <- x$getim()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setim(m)
    m
}
