## This set of functions creates a special cacheMatrix object, which can be
## solved to find its inverse. If the inverse has already been found, the
## previously computed solution will be returned.

## makeCacheMatrix creates a special matrix object in the form of a list. This
## object contains the matrix (x), a cached copy of the inverse (inv) of the 
## matrix (if one has previously been computed), and methods to set the value 
## of the matrix (set), to return the value of the matrix (get), set the value 
## of the cached inverse (setInverse), and return the value of the cached 
## inverse (getInverse).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ##define the functions governing the cacheMatrix object
        set <- function(y) {
                x <<- y
                inv <<- NULL ##Erase cached inverse if matrix value is changed                              
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        
        ##returns list containing functions defined above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                         
}

## cacheSolve takes a makeCacheMatrix object, and checks (x$getInverse) to see
## if an inverse  has been computed. If it has, the function returns the cached 
## value. If it hasn't, the function reads the matrix value (x$get), solves for
## the inverse matrix, and returns the inverse as a standard matrix object.

cacheSolve <- function(x) {   
        inv <- x$getInverse()
        
        #check the cache
        if(!is.null(inv)) {
                message("reading cached inverse...")
                return(inv)
        }
        
        #otherwise solve for the inverse
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        message("matrix inverse cached")
        inv
}