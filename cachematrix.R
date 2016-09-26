## makeCacheMatrix and cacheSolve functions work as a pair to get the inverse matrix from a matrix and save it
## into a cache for faster usability instead of using for example the solve() function repeatedly. 

## makeCacheMatrix() function is really a list containing functions to:
## 1. set the value of the matrix to be inverted
## 2. get the value of the matrix
## 3. set the inverse of a matrix into a variable
## 4. get the inverted matrix from that variable
## Note: This function does not calculate the inverted matrix at any point.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## 1. Sets the desired matrix we wish to get inverted. Also sets the 'inv' variable to be NULL at this point.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 2. Returns the set matrix
    get <- function() x
    
    ## 3. Sets a value into the variable 'inv'. CacheSolve function utilizes this to save the inverted matrix.
    setinvert <- function(solve) inv <<- solve
    
    ## 4.Returns the contents of variable inv, which is either the cached inverted matrix or NULL.
    getinvert <- function() inv
    
    ## Returns the set of functions.
    list(set = set, get = get, 
         setinvert = setinvert,
         getinvert = getinvert)
}


## cacheSolve() function calculates the inverse of a matrix if it's not found in the cache.
## If the inverse is calculated in the cache, this function returns it from the cache instead of calculating it.

cacheSolve <- function(x, ...) {
    ## Check to see if the inverted matrix was already in the cache and returning it if true.
    inv <- x$getinvert()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Calculation and returning of the matrix that is the inverse of 'x' in four steps.
    
    ## 1. Getting the data by utilizing the calls of makeCacheMatrix.
    data <- x$get()
    
    ## 2. Inverting the matrix and saving the result into the variable 'inv'.
    inv <- solve(data, ...)
   
    ## 3. Setting the inverted matrix into the cache by utilizing the calls of makeCacheMatrix.
    x$setinvert(inv)
    
    ## 4. Returning the value of the inverted matrix
    inv
        
}
