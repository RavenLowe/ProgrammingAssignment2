## Put comments here that give an overall description of what your functions do

## The following pair of functions, makeCacheMatrix and cacheSolve, work
## together computing the inverse of a square matrix and caching it to avoid 
## recomputation if we need it again

## Write a short comment describing this function

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        
        ## store the cached value
        ## set to NULL
        cached <- NULL
        
        ## set the matrix in the working environment
        ## reset cached to NULL
        set <- function(y) {
                x <<- y
                cached <<- NULL
        }
        
        ## return the value of the matrix
        get <- function() x
        
        ## set cached to the value of inverted matrix
        setInverse <- function(inverse) cached <<- inverse

        ## return the inverted matrix
        getInverse <- function() cached

        ## return the 4 functions defined above to the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## above. If the inverse has already been calculated in the working environment,
## it retrieves the inverted matrix from cache.

cacheSolve <- function(x, ...) {
        
        ## get the inverted matrix from cache
        cached <- x$getInverse()
        
        ## return a message and the inverted matrix if cached is not NULL
        if(!is.null(cached)) {
                message("getting cached data")
                return(cached)
        }
        
        ## else get the matrix from makeCacheMatrix
        matrix <- x$get()
        
        ## compute the inverted matrix
        cached <- solve(matrix, ...)
        
        ## set inverted matrix in cache
        x$setInverse(cached)
        
        ## return the inversted matrix
        cached
}
