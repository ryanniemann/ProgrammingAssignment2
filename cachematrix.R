# The assignment is regarding lexical scoping and caching 
# functions that may require a long computation time. 

# This first function creates a special 
# matrix object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        y <- NULL 
        
        setmatrix <- function(y) { 
                x <<- y 
                m <<- NULL 
        }
        getmatrix <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse,
             getinverse = getinverse)
}

# The following function returns the inverse of the matrix 

cacheSolve <- function (x, ...) {
        # first check if the inverse has already been computed 
        
        m <- x$getinverse() 
        if(!is.null(m)){ 
                return(m)
        }
                
        # if needed compute the inverse 
                
        y <- x$getmatrix() 
        x$setmatrix(y) 
        m <- solve(y, ...) 
        x$setinverse(m) 
        m 
}



