## Here are two functions defined. 
## The first one creates a list a functions from a matrix.
## The second one computes the inverse of a mtrix if it has not been 
## computed previously.



## This function takes a matrix and create a list of 4 functions.
## get function returns the matrix.
## set enables to assign a new matrix.
## getinverse function returns the inverse of the matrix, 
## if it has been previously set.
## setinverse function computes the inverse of the assigned matrix.

makeMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the getinverse and  setinverse defined previously  
## to compute the efficiently inverse of the matrix. Firstly it checks 
## if an inverse has been previously set. In that case it just returns the 
## the inverse given that it is storaged. If not, the function computes it,
## storages it and return it. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}