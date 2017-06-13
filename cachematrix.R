## Collection of functions to store matrices and compute their inverse
## When computed, the inverse is also stored for reuse and is not recalculated
## for a given data set


## Creates a list that contains the functions to store and retrive the a
## matrix and its inverese

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse as NULL
    inv <- NULL
    
    # Stores the matrix and resets the inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Retrives the matrix
    get <- function() x
    
    # Stores a value to the inverse
    setinv <- function(x_inv) inv <<- x_inv
    
    # Retrives the stored value of the inverse
    getinv <- function() inv
    
    # List of names for each function
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a matrix created by 'makeCaheMatrix'
## The result is either computed or retrived from cache

cacheSolve <- function(x, ...) {
    # Retrive inverse of the matrix
    inv <- x$getinv()
    
    # Test if inverse is already calculated and return the stored value if TRUE
    if(!is.null(inv)) {
        return(inv)
    }
    
    # Retrive and calculate the inverse if above condition was FALSE
    data <- x$get()
    inv <- solve(data, ...)
    
    # Store and return the calculated inverse
    x$setinv(inv)
    inv
}
