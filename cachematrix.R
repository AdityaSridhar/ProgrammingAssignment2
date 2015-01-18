## The objective of the following program is to cache a time consuming 
## operation on matrices, namely, obtaining the inverse of a given matrix.

## The makeCacheMatrix function takes the passed matrix and 
## returns a list which contains operations that enable caching the matrix inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    cachedInverse <- NULL
    
    set <- function(y) 
    {
        x <<- y
        cachedInverse <<- NULL 
    }
    
    get <- function() x
    
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    getInverse <- function() cachedInverse
    
    ## List containing operations to be performed on the cache matrix.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
} 


## cacheSolve returns the inverse of the passed matrix.
## The matrix passed is assumed to be of the "special" cached Matrix form.
cacheSolve <- function(x, ...) {
    
    ## Get the inverse of the cache matrix
    matrixInverse <- x$getInverse()
    
    ## If the inverse has been cached, retrieve the same.
    if(!is.null(matrixInverse)) 
    {
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## Inverse has not been cached.
    data <- x$get()
    
    ## Compute the inverse.
    matrixInverse <- solve(data, ...)
    
    ## Cache the inverse.
    x$setInverse(matrixInverse)
    
    ## Return the inverse 
    matrixInverse
}
