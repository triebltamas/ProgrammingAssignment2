## You can ask for a concrete matrix's inverse, and 
## these functions cache the result, so that next time 
## it doesnt have to compute it


## this function creates a matrix, sets it, gets it,
## same with its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function asks for a certain Martix's inverse, and if set,
## returns it, otherwise computes it, sets it, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}