## Author: Geert Van Landeghem

## This source code contains two functions:

## makeCacheMatrix: This function creates a special "matrix" object that can 
##                  cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already 
##             been calculated (and the matrix has not changed), then cacheSolve
##             retrieves the inverse from the cache.

## Usage scenario:

## 1. create a matrix
## > c = rbind(c(1, -1/4), c(-1/4, 1))
## 2. create the cached matrix
## > cm <- makeCacheMatrix(c)
## 3. use the cached matrix a first time
## > cacheSolve(cm)
##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 4. use the cached matrix a second time -> cached data is used!
## > cI <- cacheSolve(cm)
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 5. Verify inverted matrix is correct
## c %*% cI
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## makeCacheMatrix: takes a matrix and turns it into a special "matrix", which
## is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    } 
    setinverted <- function(inverted) m <<- inverted
    getinverted <- function() m
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## cacheSolve: takes a special matrix as input and will use its cached inverted
## when it exists or will calculate the inverted matrix and put it into the 
## cache of the special matrix for later use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}



