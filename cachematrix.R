## This function defines functions for program operation
##It creates a matrix object that can cache it's inverse
makeCacheMatrix <- function(m = matrix()) {
        inverse <- NULL
        set <- function(y){
                m <<- y
                inverse <<- NULL
        }
        get <- function() return(m)      
        ## These functions store value, they don't calculate it
        setinverse <- function(i) inverse <<- i
        getinverse <- function() return(inverse)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates inverse of a matrix and sets a new value to global 'i'

cacheSolve <- function(m, ...){
        inverse <- m$getinverse()
        ##Check if i already has a value
        if(!is.null(inverse)){
                message("Getting cached data")
                return(inverse)
        }
        ## Inverse the matrix 'm' and set a new value to global 'inverse'
        data <- m$get()
        inverse <- solve(data, ...)
        m$setinverse(inverse)
        ## Return a matrix that is the inverse of 'm'
        return(inverse)

}
