

## The first function, makeVector creates a special "vector", which is 
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

    mInv <- NULL
    set <- function(y) {
            x <<- y
            mInv <<- NULL
    }
    get <- function() x
    setinverse <- function(myinverse) mInv <<- myinverse
    getinverse <- function() mInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Test code

set.seed(10)

# create the matrix
mtrx <- replicate(4, rnorm(4)) 
id <- diag(nrow(mtrx))

# create CacheMatrix 
tst <- makeCacheMatrix(mtrx)

# Solve first time
mtrxinv <- cacheSolve(tst)
res <- mtrxinv %*% mtrx
print(res)

# get from cache
mtrxinv <- cacheSolve(tst)
res <- mtrxinv %*% mtrx
print(res)

# set to different matrix
mtrx <- replicate(4, rnorm(4)) 
tst$set(mtrx)

# rerun solver
mtrxinv <- cacheSolve(tst)
res <- mtrxinv %*% mtrx
print(res)

