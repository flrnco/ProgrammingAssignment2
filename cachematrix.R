## -- File cacheMatrix.R
## Contains 2 functions that cache the inverse of a matrix.


## -- Function makeCacheMatrix
## Objective : create a matrix which save in cache memory it value
##             and it inverse
##
## - how to use :
## 1 - Create an empty cacheMatrix
##   > myMatrix <- makeCacheMatrix()
## 2 - Create a cacheMatrix by copy
##   > myMatrix <- makeCacheMatrix(x)  # x : matrix you want to copy

makeCacheMatrix <- function(x = matrix())
{
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invx <<- inverse
    getinverse <- function() invx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## -- Function cacheSolve
## Objective : compute a matrix inversion, if necessary, and return
##             the result
##
##  - input : x : cacheMatrix you want to invert
##  - output : the matrix inverted (if possible)
##  - how to use :
##   > x <- makeCacheMatrix(y)  # y : an invertible matrix
##   > cacheSolve(x)
##   > x$getinverse()  # return the inverted matrix

cacheSolve <- function(x, ...)
{
    invx <- x$getinverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinverse(invx)
    invx
}
