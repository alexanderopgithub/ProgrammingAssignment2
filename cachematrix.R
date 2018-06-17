# Given square matrix A we can normally find the inverse of matrix A using R-function "solve".
# With combination of functions makeCacheMatrix and cacheSolve below we can also find the inverse of A, 
# but take it from cache whenever possible

# Here we assume the matrix A is invertible. Otherwise we could implement something like
# https://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
# For quick check on invertible matrix: A %*% solve(A)  = diag(2)    [diag(2) = identity matrix]  

# example usage
# source("cachematrix.R")
# A1 <- matrix(c(1,2,3,4),2,2)    # has inverse matrix(c(-2, 1, 1.5, -0.5),2,2) 
# A2 <- matrix(c(5,6,7,8),2,2)    # has inverse matrix(c(-4,3,3.5,-2.5),2,2)
# aMatrix <- makeCacheMatrix(A1)  # create special matrix object; initialized with A1
# aMatrix$get()                   # give back value of current matrix (A1)
# aMatrix$getinverse()            # give back value of inverted matrix (A1). But no inverse calculated yet, so value is NULL
# aMatrix$set(A2)                 # reset current matrix to A2
# cacheSolve(aMatrix)             # gives back value of inverted matrix. Now inverse is calculated (and of A2)
# aMatrix$getinverse()            # gives back value of inverted matrix. Now inverse is retrieved directly from cache
# cacheSolve(aMatrix)             # gives back value of inverted matrix. Cached
# aMatrix$set(A1)                 # reset current matrix to A1
# cacheSolve(aMatrix)             # gives back value of inverted matrix. Now inverse is calculated (and of A1)
# cacheSolve(aMatrix)             # gives back value of inverted matrix. Now inverse is retrieved directly from cache


# create methods (functions set/get/setinverse/getinverse) for inverse calculation using special matrix object 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# function calculates inverse of special matrix from makeCacheMatrix if the inverse has not yet been calculated.
# if inverse has been calculated before, function returns the cached version of the inverse.
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

