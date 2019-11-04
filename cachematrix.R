## In this assignment, I will cache the inverse of a matrix 
## since it is more beneficial to do so than to compute the matrix repeatedly.


## makeCacheMatrix is a function in which the special "matrix" object created can cache its inverse.
## This function takes an input matrix and returns a list. 
#1.  x is the input matrix, the default is an empty matix.
#2.  The first step is define the function set that sets the initial value of x and m.  The <<- operator means this information is called object in an environment that is different from an enviroment different from the current function.
#3.  The 2nd step defines the get function as makine equal to the input matrix
#4.  The next steps assign the inverse of the matrix to m
#5.  Lastly, the output list is created.  set data (x & m), get (input matrix), setsolve (the inverse), and getsolve (new cached m data)
#6.  By naming the objects in the list, $ can be used to select parts.

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
## If the original matrix has been previously tested then cached data is returned along with a note.  
## If not this functin calculates the inverse.
#1.  The first step is to determine if there is cached data.  If so a note and the cached data is returned
#2.  The next step retuns the inverse if not cached

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
