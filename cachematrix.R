## Ranxel Almario's submission for the 2nd assignment 

## Creates a special "matrix" object that can cache its inverse
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
        ## Return inverse of the current 'x'  whether empty or not
        cx <- x$getinv()
        
        ## Check if x (cx) is already set or is NULL
        if(!is.null(cx)) {
                message("Getting cached data...")
                return(cx)
        }
        
        ## Get the value of the current 'x' matrix for computation
        datax <- x$get()
        
        ## Inverse the 'datax' matrix
        ninv <- solve(datax)
        
        ## Set the inverse cache for future use
        x$setinv(ninv)
        
        ## Return the inverse
        ninv
}

## Sample tests included:

## > sampleMatrix <- rbind(c(1, -1/4), c(-1/4, 1))  
## > sampleMatrix
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > resultMatrix <- makeCacheMatrix(sampleMatrix)
## > resultMatrix$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > resultMatrix$getinv()
## NULL
## > sampleSecondMatrix <- rbind(c(1, -1/2), c(-1/2, 1))
## > sampleSecondMatrix
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > resultMatrix$set(sampleSecondMatrix)
## > resultMatrix$get()
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > solve(sampleSecondMatrix)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > cacheSolve(resultMatrix)
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > cacheSolve(resultMatrix)
## Getting cached data...
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333