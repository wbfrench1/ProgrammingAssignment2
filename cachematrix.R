## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # set the default null value for inv_x
        inv_x <- NULL
        # write the set function
        set <- function(new_matrix) {
                # assign the new_matrix variable to x
                x <<- new_matrix
                # assign default null value for inv_x
                inv_x <<- NULL
        }
        # write the get function to return x
        get <- function() x
        # write the set function to define inv_x
        set_inv_x <- function(inv) inv_x <<- inv_x
        # write the get function to get the value of inv_x
        get_inv_x <- function() inv_x
        # create a list of the functions components to return 
        list(set = set, get = get,
             set_inv_x = set_inv_x,
             get_inv_x = get_inv_x)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get inv_x varibale from the makeCacheMatrix list
        inv_x <- x$get_inv_x()
        # check to see of inv_x is defined
        if(!is.null(inv_x)) {
                message('getting cached data')
                return (inv_x)
        }
        # get the data associated with the supplied makeCacheMatrix list
        data <- x$get()
        # solve for the inverse matrix
        inv_x <- solve(data)
        # set the value of the makeCacheMatrix list 
        x$set_inv_x(inv_x)
        # return the inverse of the matrix
        inv_x
}
