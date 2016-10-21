## Lesson 2 Week 3 Programming Assignment
## Matrix inversion caching function
## Chris Drumgoole, October 2016

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL # solved value (was m in vector mean example)
    set <- function(y) { # Constructor that stores supplied matrix
        x <<- y
        s <<- NULL
    }
    get <- function() x # Returns cached original that is stored
    setSolved <- function(solve) s <<- solve # stores the solved, or inverse of the (cached) matrix
    getSolved <- function() s # returns the solved, or inverse of the (cached) matrix
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved) # storage
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolved() # store the solved (inverse) matrix of original matrix in s
    if(!is.null(s)) { # here we check if the solved matrix has been cached. if yes, then we simply return it
        message("getting cached data")
        return(s)
    }
    
    # We dont have the inverse of the matrix cahced, so let's continue
    data <- x$get() # get the matrix
    s <- solve(data, ...) # solve (inverse) the matrix (and cache the matrix), store in s
    x$setSolved(s) # cache the solved matrix
    s
}



### 
### EXAMPLE CODE FOR VECTOR MEAN CACHING
###

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# Make test vector 1
vector1 <- makeVector(c(1,2,3,4))
cachemean(vector1) # test cachemean once
cachemean(vector1) # test cachemean twice

# Make test Matrix 1
matrix1 <- matrix(c(1,2,3,4), 2, 2) # text 2 x 2 matrix
solve(matrix1) # expected inverse output
cachedMatrix1 <- makeCacheMatrix(matrix1)
cacheSolve(cachedMatrix1) # test cacheSolve once
cacheSolve(cachedMatrix1) # test cacheSolve twice

# hilbert example
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8)
h8
solve(h8)
cachedh8 <- makeCacheMatrix(h8)
cacheSolve(cachedh8)
cacheSolve(cachedh8)
