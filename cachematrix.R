## Put comments here that give an overall description of what your
## functions do


## This function creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    p <- NULL # pulled in here to be used as needed later in function
    
    ## Create a function to build the matrix
    set <- function(y) {
        x <<- y     # assigns y argument to x object in parent env.
        p <<- NULL  # assigns NULL to p object in parent env. to clear out any previous values (supports caching)
    }
    
    get <- function() x  # allows x to be pulled from the parent env.
    
    ## Create matrix object
    setmatrix <- function(matrix) p <<- matrix # assigns matrix argument to the p object in the parent env.
    getmatrix <- function() p   # allows p to be pulled from the parent env. 
    
    
    ## Assemble above functions into one list object to be returned to the parent env.
    # Names each element using "elementName = value" syntax to support subsetting as needed
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}

## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'p'
        p <- x$getmatrix()     # calls the getmatrix function on the input object
        
        # checks if result is NULL and, if NOT, returns the cached matrix p to the parent env.
        if(!is.null(p)) {
            message("getting cached data")
            return(p)
        }
        
        # if result is NULL:
        mtx <- x$get()          # gets matrix from input object get()
        p <- solve(mtx, ...)    # determines the inverse of the cached matrix and assigns it to object p
        x$setmatrix(p)          # sets the inverse of the cached matrix as the input object p
        p                       # returns the inverse matrix (object p) to the parent env. and prints the output
}



### TO TEST
t1 <<- matrix(data = c(2,4,1,3), nrow = 2, ncol = 2)
##Inverse - solve(t1)
    #   [,1] [,2]
    #   [1,]  1.5 -0.5
    #   [2,] -2.0  1.0


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
#Expected outputs:  m1 %*% n1 = I2

myMatrix_object <- makeCacheMatrix(m1)  # Creates matrix object
cacheSolve(myMatrix_object)     # Returns inverse of matrix object
                                # Run again to test that cached data is pulling as expected
myMatrix_object$set(n2) # set another matrix to be inverted to test functions are working well
