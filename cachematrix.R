## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special matrix and has 4 functions included 
## cacheSolve creates the inverse matrix of the matrix created in makeCacheMatrix


## Write a short comment describing this function
## The first function sets the value of the matrix 
## The second gets the value of the matrix
## The fourth sets the inverse of the matrix 
## The forth gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## It first checks if the inverse matrix has been calculated already. 
## If so, it is not calculated again but called from the cache and a text message "getting cached data" and the inverse matrix from the cache is printed 
## If not so the inverse matrix is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setsolve(i)
        i
}