## Compute and cache the inverse of matrices

## The function makeCacheMatrix create an object that contains 4 methods:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## The function cacheSolve first checks if the inverse of x is calculated. 
## If not, calculate and store it.
## Where x is the "matrix" created from the last function. 


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inverse <- solve(matrix,...)
    x$set_inverse(inverse)
    inverse
}
