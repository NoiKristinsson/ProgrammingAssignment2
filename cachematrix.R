## March 2015 :D
## Creates a cacheable matrix for inputting to
## cacheSolve() function which sets and gets the cached values
## Taking advantage of the scoping rules of the R programming language 

## A function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        # Prepare the inverse value (inv_val) with NULL
        inv_val <- NULL
        
        # set value of the matrix
                set <- function(y) {
                        x <<- y
                        inv <<- NULL # matrix has changed, reassign to NULL
                }
        
        # get value of matrix
        get <- function() x
        
        # set inverse (set_inv) of matrix
        set_inv <- function(inverse) inv_val <<- inverse
        
        # get inverse (get_inv) of matrix
        get_inv <- function() inv_val
        
        # return a list containing all functions defined above and name given to them
        list(set = set, get = get,
             setinverse = set_inv,
             getinverse = get_inv)
}



## cacheSolve(x, ...) - where 'x' is a 'makeCacheMatrix' object.
##  Returns the inverse of a matrix returned from makeCacheMatrix
## If it has been calculated, it will be returned from cache

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x'
        # get inverse (put it into inv.val)
        inv.val <- x$getinverse()
        
        # if inverse exists -> it will check if it is already cached
        # if -> yes than -> return cached inverse
        if(!is.null(inv.val)) {
                message("retrieving cached data")
                return(inv.val)
        }
        
        # if not, get it gets the matrix
        data <- x$get()
        
        # computes inverse of the matrix
        inv.val <- solve(data, ...)
        
        # caches inverse of the matrix
        x$setinverse(inv.val)
        
        # return inverse of the matrix
        inv.val
}

