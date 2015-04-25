## Since Matrix inversion is a costly computation, it is beneficial to cache
## the inverse of a matrix rather than computing it repeatedly.
## The two functions below takes a matrix (x), computes it inverse and stores 
## the inverse in the cache


## "makeCacheMatrix" function creates a special "matrix" and returns a list of 
## functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## "cacheSolve" function returns a matrix that is the inverse of 'x'.
## This function will first check whether the inverse is already computed. 
## If true, it will return inverted matrix along with a message 
## "getting cached data".
## If the inverted matrix is not already computed, it calculates the inverse 
## matrix and stores the result in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x' and cache it
}
