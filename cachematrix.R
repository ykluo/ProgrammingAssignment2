## In this file, there are 2 functions to calculate the inverse
## of a matrix. 

## The first function, called makeCacheMatrix, will create a 
## special matrix, by taking a real matrix (inversable)
## as input parameter and returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
    ## This varible i stores inverse of this matrix, if colculated
    i <- NULL
    
    ## Set function, change the value of this matrix to a new one
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get function, get current matrix value
    get <- function() x
    
    ## Set the inverse, store in i. Used in cacheSolve function
    set_inverse <- function(inv) {
        i <<- inv
    }
    
    ## Get inverse, return stored inverse value i
    get_inverse <- function() i
    
    ## Return the four-function list
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function will take a special 'matrix' created
## by makeCacheMatrix function above as input parameter
## and return its inverse value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$set_inverse(i)
    i
}
