## Functions to cache a matrix inverse by creating an  
## object able to store its inverse.


## This function creates an object (consisting of a list 
## of functions and two global variables) able to store and 
## retrieve both a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        # Set the inverse to NULL when creating the CacheMatrix
        # in this environment
        ivs <- NULL
        
        # Set function that stores the matrix and its inverse in  
        # the global environment
        set <- function(y) {
                x <<- y
                ivs <<- NULL
        }
        
        # Get function that returns the matrix
        get <- function() x
        
        # function to store the inverse (in the global environment)
        setinverse <- function(inverse) ivs <<- inverse
        
        # function to retrieve the inverse
        getinverse <- function() ivs
        
        # This function returns a list with the two setters
        # and the two getters, accesible by name
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns a matrix's inverse. If the inverse has 
## been calculated previously, it returns the inverse from the
## cache. It takes as arguments an object created by using the
## function makeCacheMatrix and any extra arguments for solve()
## aside the "a" and "b" (If extra arguments are used, they won't
## be taken into account when calling again cacheSolve in
## succesive calls, even if they are different)
## If the numeric matrix is not invertible, an error will be 
## produced

cacheSolve <- function(x, ...) {
  
        # Try to get the inverse from the cache
        inverse <- x$getinverse()
        
        # If it is already in the cache, return it
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        # The inverse is not in the cache
        # Get matrix
        data <- x$get()
        
        # Solve for the inverse matrix
        # assuming that the matrix supplied is always invertible
        inverse <- solve(a = data,b = diag(dim(data)[1]), ...)
        
        # Store the inverse matrix via the set function
        x$setinverse(inverse)
        
        # Return the inverse matrix
        inverse     
}
