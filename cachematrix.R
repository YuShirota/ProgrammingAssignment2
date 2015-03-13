# *** Calculation of an inverted matrix for a given matrix M ***
# If the inverse is already solved, these functions return
# the "cached" result.
## First, a matrix M is passed to the makeCacheMatrix function,
## where a preparation for cache-ing is performed.
## Then, the resulting object serves as an argument for the cacheSolve function.
## If an inverted matrix for the same object (hence the same matrix M) is
## already calculated, cacheSolve will return the cached result
## assigned to "inv"


## Prepare to "cache" the inverted matrix of x for cacheSolve function.
## Returns a list of functions for setting/getting the matrix, 
## and setting/getting the inverted matrix. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL    # inv will contain the inverted matrix
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
            setinverse = setinverse, getinverse = getinverse)
}


## Calculating inverted matrix passed to the makeCacheMatrix funtion
## If the same calculation was already done, it will return the
## cached data.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x',
    ## which is returned by makeCacheMatrix function above.
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    if (det(data) == 0){   ## Optional code to check if the matrix can be inverted
        message("The matrix cannot be inverted!!")
        return(NULL)
    } else {
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
}