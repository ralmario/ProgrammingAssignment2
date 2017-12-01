## Ranxel Almario's submission for the 2nd assignment 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inv in the makeCache environment
        inv <- NULL
        
        ## Set the value of the matrix
        set <- function(nx) {
                x <<- nx
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() {
                ## Return the current value of x
                x
        }
        
        ## Set the value of the inverse of x matrix
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        ## Get the value of the inverse of x matrix
        getinv <- function() {
                ## Return the current value of inv
                inv
        }
        
        ## Return all methods in a list
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by makeCache funct
## If already calculated, then cacheSolve should retrieve the inv cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
