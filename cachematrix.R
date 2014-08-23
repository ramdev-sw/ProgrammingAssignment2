#This program uses lexical scoping to simulate cache. The cache stores a matrix and inverse of the matrix.
#The cache is part of the environment preserved in the functions returned by makeCacheMatrix() function.



#This function defines and returns 4 functions : set, get, setinverse and getinverse.
#set function assigns the matrix to be inverted to the cache.
#get function reads the matrix to be inverted from the cache.
#setinverse function assigns the inverted matrix to the cache.
#getinverse function reads the inverted matrix from the cache.
#The above functions that is returned as part of list preserve the environment in which the functions are
#defined, argument 'x' and variable 'i' which caches the matrix to be inverted
#and the inverted matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of a matrix, using solve.  
#This function invokes the functions returned by makeCacheMatrix() to determine if the inverse is already calculated. 
#If inverse is not calculated, this function computes the inverse and sets the cache using setinverse function(returned by makeCacheMatrix).

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
