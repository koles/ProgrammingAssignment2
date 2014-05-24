#' Create a list wrapping a given matrix; the returned list includes
#' utility methods for caching an inverse matrix. The list is expected
#' to be used as an argument to the cacheSolve method.
#'
#' @param x a matrix to be wrapped and eventually inverted using the
#'   cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

#' Returns inverse matrix of the matrix wrapped by a makeCacheMatrix function
#'
#' @param x a list returned by the makeCacheMatrix function. x$get must return
#'   a matrix
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
