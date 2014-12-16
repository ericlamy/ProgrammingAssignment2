##
## makeCacheMatrix function creates a "matrix" object that can cache its inverse.
## Rem: we assume that the matrix supplied is always invertible.
##
## Input: A square and invertible matrix
##
## Output: A list of 4 elements which are the definition of the methods of
##         the matrix object: set, get, setminv and getminv.
##
## Version:
##      1.0 141216 Inital version (Eric)
##
## Todo: Test if input is valid (square invertible matrix)
##
makeCacheMatrix <- function(x = matrix()) {
    cacheM = matrix(data=NA) # Define and initialize the matrix cache
    #
    set<-function(Mymat){
        x <<- Mymat         # Push argument in variable x of the calling env. (parent frame)
        cacheM <<- matrix(data=NA) # Initialize the cache in calling env.
    }
    #
    get<-function() x       # Pop x from the calling env.
    #
    setminv<-function(minv) cacheM <<- minv # Push matrix inverse in cache in calling env.
    #
    getminv<-function() cacheM              # Pop matrix inverse from cache
    #
    list(set=set, get=get, setminv=setminv, getminv=getminv)
}
##
## cacheSolve function computes the inverse of the "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache (variable cacheM).
##
## Input: A square and invertible matrix + ...
##
## Output: A matrix that is the inverse of input 'x'
##              (if the inverse matrix has already been computed,
##              it is retrieved from the cache matrix, for efficiency)
## Version:
##      1.0 141216 Inital version (Eric)
## Todo:
##
cacheSolve <- function(x, ...) {
    cacheM <- x$getminv()   # Invoke 'getminv' of matrix object x and store in inverse cache.
    if (!is.na(cacheM[1,1])) { # Inverse matrix has alreday been calculed.
        message("Getting cached inverse matrix.")
        return(cacheM)
    }
    tempM<-x$get()          # Invoke 'get' method of matrix object x.
    cacheM<-solve(tempM, ...)   # Compute inverse matrix and caches.
    x$setminv(cacheM)        # Store inverse matrix in matrix cache.
    cacheM
}
