### Introduction

### This second programming assignment will require you to write an R

### This function will cache potentially time-consuming computations Matrix for inverse by 
### cacheSolve

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
    set <- function(y) 
        {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





### This function computes the inverse of the special
### "matrix" returned by `makeCacheMatrix` above. If the inverse has
###  already been calculated (and the matrix has not changed), then the
###  `cachesolve` will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getinverse()
    if(!is.null(inv)) 
        {
        message("Getting inverse from cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
