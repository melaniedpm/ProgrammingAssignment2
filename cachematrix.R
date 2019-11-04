## In this assignment, I will cache the inverse of a matrix 
## since it is more beneficial to do so than to compute the matrix 
## repeatedly.


## makeCacheMatrix is a function in which the special "matrix" object created can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) { #assigns the input argument to the x object in the parent environment
                x <<- y
                inv <<- NULL #assigns the value NULL to the i object in parent environment
        }
        get <- function() x #retrieve x from the parent environment of makeCacheMatrix()
        setInverse <- function(inverse) inv <<- inverse #defines the setter for the inverse i
        getInverse <- function() inv
        list(set = set, #assign each of these functions as an element within a list()
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of makeCacheMatrix's matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse() #retrieve the inverse from the object passed in as the argument
        if (!is.null(inv)) { #check to see whether the result is NULL
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #get the matrix from the input object
        inv <- solve(mat, ...)
        x$setInverse(inv) #set the inverse of the input object
        inv
}
