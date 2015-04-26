## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        
        inv <- NULL                                     # 1. set the value of the matrix
        set <- function(matrix) {
                m <<- matrix
                inv <<- NULL
        }
       
        get <- function() m                             # 2. get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse # 3. set the value of inverse of the matrix
        getInverse <- function() inv                    # 4. get the value of inverse of the matrix
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated,
## then the "cachesolve" should retrieve the inverse from the cache.

# Assuming that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()     # Return a matrix that is the inverse of 'x'
                                # Just return the inverse if its already set
        if(!is.null(m)) {
                message("--- getting cached data ---")
                return(m)
        }
        
        data <- x$get()         # Get the matrix from our object
        m <- solve(data)        # Calculate the inverse using matrix multiplication
        x$setInverse(m)         # Set the inverse to the object
        m                       # Return the matrix
}


## --------   TEST

##> source("ProgrammingAssignment2/cachematrix.R")
##> x = rbind(c(1, -1/4), c(-1/4, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
##> cacheSolve(m)
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(m)
##--- getting cached data ---
##        [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667

## --------   end TEST 