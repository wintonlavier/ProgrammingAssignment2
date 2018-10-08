## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## 1.set the value of the matrix
## 2.get the value of the  matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

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
             gettmean = gettmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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


## Description verbiage comes from "Assignment: Caching the Inverse of a Matrix" found in README.RMD.
