
## This function defines functions for program operation
##nrows, ncols problem
makeCacheMatrix <- function(x = matrix()) {
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x      
        ## These functions store value, they don't calculate it
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates inverse of a matrix and sets a new value to global 'i'

cacheSolve <- function(x, ...){
        i <- x["getinverse()"]
        ##Check if i already has a value
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ## Inverse the matrix 'x' and set a new value to global 'i'
        else{
                data <- x["get()"]
                i <- solve(data, ...)
                x["setinverse(i)"]
                i
        }
        ## Return a matrix that is the inverse of 'x'
}
