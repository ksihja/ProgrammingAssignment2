##
## Fuctions makeCacheMatrix and cacheSolve are ment to be used
## for calculating matrix inversion in situations where same inverted
## result might be needed again.
##
## Usage: create matrix to be inverted by calling
##
## m <- makeCacheMatrix(<matrix>) , where matrix is your matrix object
## 
## Calculate matrix inversion by calling 
## 
## cacheSolve(m)
##
## In case inversion is already calculated for this matrix it is returned
## from cache instead of recalculating.
##
## When new matrix needs to be inverted it can be done by setting new
## matrix with call: m$set( <newMatrix) )
##
## makeCacheMatrix notices attempts to set same matrix and uses cached instance
## also these situations. Example
##
##  m <- makeCacheMatrix(matrix(1:4,2,2))
##  cacheSolve(m)   (cache populated in this call)
##
## If m$set is called after these to commands with any matrix object 
## containing exactly same colums and rows than m above cache is still kept:
##
##  m$set( cbind(1:2,3:4)) cache is kept and matrix is not updated
##  m$set(rbind(c(1,3),c(2,4)))  cache is kept and matrix is not updated
##
## Testing inverted result should always result a identity matrix :
## > cacheSolve(m1) %*% m1$get()
## Getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## FUNCTION makeCacheMatrix: creates cache for matrix to be inverted.
## Caching is based on set function created inside makeCacheMatrix
## which updates or preserves possibly already calculated inverted
## matrix (matrixinverse).
## Matrixinverse is parameter in makeCacheMatrix closure so deep
## binding parameter ( <<- ) is used to clear this from inner
## function set in case different matrix is offered for makeCacheMatrix.
## Deep binding is needed so that inner set() function can access matrixinverse
## in it's parent environment defined inside makeCacheMatrix function body.
## 
## This function offers also getters and setters for matrix and it's inversion
## as inner function.
## 

makeCacheMatrix <- function(x = matrix()) {
    message("Calling makeCachematrix")
    matrixinverse <- NULL
    set <- function(y){
        ## We use all.equal function to compare whether
        ## matrix to be set here is already saved into
        ## special vector.
        ## all.equals returs either TRUE (if matrix are equal in the sense
        ## that each element in both matrices are equal )
        ## or vector of "mode" characters in case they are not equal.
        ## We set tolerance explicitly to 0 to identify exact matches
        ## of matrices.
        ## For more see documentation of all.equal()
        ## Thus condition to update matrix and clear cache is situation
        ## when we do not get TRUE as a return value.
        ## isTRUE handles corretly also mode characters and returns
        ## FALSE if matrices are unequal.
        if(!isTRUE(all.equal(x,y, tolerance = 0))){
            message("Matrix changed ... updating it and setting inverse to NULL")
            x <<- y
            matrixinverse <<- NULL
        }
    }
    ## Get special matrix
    get <- function() x
    ## Set calculated inverted matrix into cache
    setinverse <- function(inverse) matrixinverse <<- inverse
    ## Get calculated inverted matrix from cache
    getinverse <- function() matrixinverse
    ## Define names for the inner functions
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## FuNCTION cacheSolve: Argument x is a special matrix created 
## by calling makeCacheMatrix above.
## Function returns an inverted matrix of 'x'
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check if we have readily calculated inversion 
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        ## Inversion calculated in cache, return it without recalculation
        message("Getting cached data")
        return( inverse )
    }
    # No inversion found in cache, calculate it 
    message("Calculating inverse")
    matrix <- x$get()
    inverse <- solve(matrix)
    ## Populate inversion cache
    x$setinverse(inverse)
    inverse
}
