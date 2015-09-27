#Creating a function to return the Required functions for the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
 
        setMatrix <- function(matrix = matrix()){
                x <<- matrix
        }
 
        getMatrix <- function() x
 
        setInverse <- function(inverseMatrix = matrix()){ 
                inverse <<- inverseMatrix
        } 
        getInverse <- function() inverse
        list(get = getMatrix, set = setMatrix, getI = getInverse, setI = setInverse)
}
 
 
## checks if the given makeCacheMatrix object already has it's inverse calculate.
## If not, it calculates it's inverse and caches it
 
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
