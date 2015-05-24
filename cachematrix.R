## This function does the following
## 1. Sets the value of a matrix (set)
## 2. Gets the value of a matrix (get)
## 3. Sets the value of the inverse (setinv)
## 4. Gets the value of the inverse (getinv)

## Additional note: for a matrix that is not square, the function "ginv" can be used.
## The function ginv requires the package MASS; to modify the code appropriately,
## we would require MASS, and replace "solve" with "ginv".

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

         setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix not changed, then it retrieves the already calculated inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


## The code below allows you to test the two functions. (Just uncomment out the code beginning with the assignment to "test".
## "test" contains a 10x10 matrix, testCached generates the makeCacheMatrix object, and testCached either calculates or retrieves the calculated matrix inverse
##
##  test <- matrix(rnorm(100),10,10)
##  testCache <- makeCacheMatrix(test)
##  testInv <- cacheSolve(testCache)
