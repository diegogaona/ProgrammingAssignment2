## Using the provided example I've modified the code to create a "matrix" object that can cache its inverse.

## This function allows to cache the inverse of a matrix, the function returns a list of functions 

makeCacheMatrix <- function(x = matrix()) 
{
    ## Initializing the matrix var to null
    matrix <- NULL
    
    ## set the matrix's cache to NULL
    set <- function(y)
    {
        x <<- y
        matrix <<- NULL
    }
    
    ## return the matrix 
    get <- function()
    {
        x
    }
    
    ## caches the solve which is the inverse of the provided matrix (x)
    setSolve <- function(solve)
    {
        matrix <<- solve    
    }
    
    
    ## returns the inverse of the provided matrix
    getSolve <- function()
    {
        matrix
    }
    
    ## Returns a list containing the functions
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
    
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) 
{
    ## Returns the inverse of the provided matrix
    ## To find the inverse using the getSolve function part of the object x
    matrix <- x$getSolve()
    
    if(!is.null(matrix)) 
    {
        ## Displaying the "getting cached data" message
        message("getting cached data")
        
        ## Returns a cached inverse matrix
        return(matrix)
    }
    
    ## set the data var as the provided matrix
    data <- x$get()
    
    ## getting the inverse of the matrix with the solve function
    matrix <- solve(data, ...)
    
    ## Caches the inverse of the given matrix
    x$setSolve(matrix)
    
    ## Returns the matrix's inverse
    matrix
} 
