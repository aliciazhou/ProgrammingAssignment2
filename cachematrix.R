## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly. 
## This programming assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## init inverse object ivs:
    ivs <- NULL
    
    ## assign x to y
    set <- function(y) {
        x <<- y
        ivs <<- NULL
    }
    
    ## gets the value of the inverse
    get <- function() x
    
    ## set inverse result from inverseMatrix input
    setinverse <- function(inverseMatrixIn) ivs <<- inverseMatrixIn
    
    ## get inverse 
    getinverse <- function() ivs
    
    ## return a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

## For this assignment, assume that the matrix supplied is always invertible.

## The x in the below function is the list of functions, which is the result 
## by calling makeCacheMatrix()
cacheSolve <- function(x, ...) {
    ## To return the inverse of the matrix . x$getinverse is the 4th column of x.
	## It will return NULL if it doesn't exist.
    inverseMatrix <- x$getinverse()
    
    ## To get old matrix, the 2nd column of x.
    data <- x$get()
    
	## Check if the inverse has already been calculated. 
    if(!is.null(inverseMatrix)) {        
        message("getting cached data")
        return(inverseMatrix)
    }
   
    message("The first time to calculate the inverse.")
    inverseMatrix <- solve(data)
    x$setinverse(inverseMatrix)
    
    inverseMatrix
	
}
