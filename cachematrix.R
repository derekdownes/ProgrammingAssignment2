## These are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        ## initialize object i with value NULL
        i <- NULL
        
        ## Assign input argument to the x object in the parent environment
        ## Assign value of NULL to the i object in parent environment
        set <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        
        ## Get x from parent environment
        get <- function() x
        
        ## Assign the input arguments to i in the parent environment
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        # Assign each function to the element of a list and return to parent
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## If calculated inverse exists in cache (i.e. not NULL)
        ## Return valid cached inverse to parent environment
        if(!is.null(i)) 
        {
                message("getting cached data")
                return(i)
        }
        
        ## Obtain matrix from input object
        data <- x$get()
        ## Solve for matrix inverse
        i <- solve(data, ...)
        ## set the inverse for the input object
        x$setinverse(i)
        ## Return the value of the matrix inverse
        i
}
