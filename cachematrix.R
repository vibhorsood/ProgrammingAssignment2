## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## 
## This function creates a special "matrix" object that can cache its inverse.
## This function also creates a list containing a function to:
## set the value of the matrix.
## get the value of the matrix.
## set the value of the matrix inverse.
## get the value of the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix_inverse <<- solve
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above.
##
##  If the inverse has already been calculated (and the matrix has not changed),
##  then this fuction retrieves the inverse from the cache and returns the 
##  inverse else it calulates the inverse by calling the solve function and
##  sets it in the cache by calling the setinverse function in the 
##  makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached matrix inverse")
                return(matrix_inverse)
        }
        matrix <- x$get()
        matrix_inverse <- solve(matrix, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
        
}
