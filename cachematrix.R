## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(new_matrix) {
                x <<- new_matrix
                inv_x <<- NULL
        }
        get <- function() x
        set_inv_x <- function(inv) inv_x <<- inv_x
        get_inv_x <- function() inv_x
        list(set = set, get = get,
             set_inv_x = set_inv_x,
             get_inv_x = get_inv_x)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inv_x()
        if(!is.null(inv_x)) {
                message('getting cached data')
                return (inv_x)
        }
        data <- x$get()
        inv_x <- solve(data)
        x$set_inv_x(inv_x)
        inv_x
}
