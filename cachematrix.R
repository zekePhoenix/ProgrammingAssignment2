## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y  ## assign the input argument to x
        inv <<- NULL  ##assign NULL to the inv object
    }
    get <- function() x ## retrieves symbol X from parent enviroment
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,  # gives the name 'set' to the set() function defined above,  # gives the name 'get' to the get() function defined above
         setinverse = setinverse,  # gives the name 'setinverse' to the setinverse() function defined above
         getinverse = getinverse)  # gives the name 'getinverse' to the getinverse() function defined above
    
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() ## function attempts to retrieve a inverse from the object passed in as the argument
    if(!is.null(inv)) { ## checks to see whether the result is NULL. Since makeCacheMatrix() sets the cached inverse to NULL whenever a new matrix is set into the object, if the value here is not equal to NULL, we have a valid, cached inverse and can return it
        message("getting cached data")
        return(inv)
    }
    data <- x$get()      ## If the result of !is.null(inv) is FALSE, cacheSolve() gets the matrix from the input object, calculates 
    inv <- solve(data)   ## a solve(), uses the setinverse() function on the input object to set the inverse in the input object, 
    x$setinverse(inv)    ## and then returns the value of the inverse to the parent environment by printing the inverse object.
    inv
}
