## Programming Assignment 2: Caching the Inverse of a Matrix
## makeCacheMatrix will cahce a matrix on its reverse
## cacheSolve If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that will cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
            x <<- y
            m <<- NULL #Indicates that the inverse was not been calculated yet
        }
        
        ##It will retrieve the matrix data
        get <- function() x
        
        ##Set the matrix to its inverse values
        setMatrix <- function(solve) m <<- solve
        
        ##Retrieves the new inverse matrix
        getMatrix <- function() m
        
        #Return the new lists
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

## Return a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
    m <- x$getMatrix() #Retrieve what is in cache
    
    ##If a inverse matrix was found, just retrieve it
    if(!is.null(m)) {
        message("Retrieving cached matrix")
        return(m)
    }
    
    ##If nothing was found in cache, then it will set its inverse
    data <- x$get()
    
    ##Set the matrix to its inverse values
    m <- solve(data, ...)
    
    ##Caches it
    x$setMatrix(m)
    
    m
}
