## This file contains 2 functions, makeCacheMatrix() & cacheSolve().
## The purpose of these functions is the ability to compute & 
## return the inverse of a square matrix the first time; and to 
## return the cached result (instead of recomputing) on subsequent 
## calls of the cacheSolve() function. The matrix (and its cached 
## inverse) are represented as an object created by the 
## makeCacheMatrix() function. 
## Computing the inverse of a square matrix is done with the 
## solve() function in R. It is assumed that the matrix supplied 
## is always invertible.

## Example usage:
## A <- matrix(c(4,3,3,2), nrow=2, ncol=2) ## creates a square matrix
## AA <- makeCacheMatrix(A) ## creates a "matrix" object out of A
## cacheSolve(AA) ## computes the Inverse of the matrix
## cacheSolve(AA) ## retrieves the Inverse of the matrix from cache


## makeCacheMatrix(): This function takes a square matrix (assumed
## invertible) and creates a special "matrix" object that can 
## cache its inverse. This function is loosely
## similar to a public class in OOP. It creates a special 
## "matrix" (akin to an object in OOP) that is really a list
## containing the following functions (aking to methods in OOP)
## 1. set() :   this stores/sets the square matrix whose inverse 
##              needs to be computed; within the special "matrix"
##              object.
## 2. get() :   this retrieves/gets the square matrix whose inverse 
##              needs to be computed; rom within the special 
##              "matrix" object.
## 3. setInverse():this sets/stores the value of the inverse matrix;
##              within the special "matrix" object.
## 4. getInverse():this retrieves/gets the value of the inverse matrix;
##              from within the special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               ## initialize the cached inverse value 
                            ## (loosely similar to a public property
                            ## in OOP) to NULL
    
    set <- function(y) {    ## This is akin to the class setter method
        x <<- y             ## Store a new input matrix
        m <<- NULL          ## Reset the cached inverse value to null
    }
    
    get <- function() x     ## This is akin to the class getter method
                            ## This simply retrieves the matrix stored
                            ## using the set() function
    
    setInverse <- function(inverse) m <<- inverse   ## Stores/caches the
                                                    ## inverse
    
    getInverse <- function() m  ## Retrieves the cached inverse
    
    list(set = set, get = get,  ## Returns the "matrix" object
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve(): This function takes a "matrix" object created using
## the makeCacheMatrix() function, and returns the inverse of the 
## matrix. The inverse is computed the first invocation (for a given 
## "matrix" object) using the solve() R function and is cached within
## the same object. For subsequent invocations, the inverse is simply
## retrieved from the cached value within the object, and returned. 

cacheSolve <- function(x, ...) {    ## Returns a matrix that is the 
                                    ## inverse of 'x'
    m <- x$getInverse()             ## Try to get the cached inverse
    
    if(!is.null(m)) {               ## Cache found!
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()         ## Get the matrix data, since no cache found
    m <- solve(data, ...)   ## Compute the inverse using solve()
    x$setInverse(m)         ## Cache the inverse for the future
    m
}

