# Function to create a special matrix object that can cache its inverse
makecachematrix <- function(mat = matrix()) {
        inv <- NULL
        
        set <- function(matrix) {
                mat <<- matrix
                inv <<- NULL
        }
        
        get <- function() mat
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# Function to compute the inverse of the special matrix
cacheSolve <- function(matrix, ...) {
        # Retrieve the cached inverse if available
        inv <- matrix$getInverse()
        
        # If the cached inverse is not null, return it
        if (!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        
        # Otherwise, compute the inverse and cache it
        data <- matrix$get()
        inv <- solve(data, ...)
        matrix$setInverse(inv)
        inv
}

