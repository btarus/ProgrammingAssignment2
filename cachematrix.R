## Function: "makeCacheMatrix"
## This function creates a special"matrix" object that caches its inverse
## This function is a list of four functions:
##    1. "set"    - set the elements of the matrix
##    2. "get"    - get the elements of the matrix
##    3. "setInv" - set the inverse of the matrix
##    4. "getInv" - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL

    # 1. changes the matrix stored in the main function
    set <- function(y) {
        x    <<- y       # assign the value of the new matrix y to x
        xInv <<- NULL    # restores to NULL the value of the old matrix inverse xInv
    }
    # 2. return the matrix stored in the main function
    get <- function() {
        x
    }
    # 3. store the matrix inverse in the main function
    setInv <- function(inv) {
        xInv <<- inv
    }
    # 4. return the matrix inverse stored in the main function
    getInv <- function() {
        xInv
    }

    # return the matrix type list of functions
    list(set    = set,
         get    = get,
         setInv = setInv,
         getInv = getInv)
}

############

## Function: "cacheSolve"
## This function computes the inverse of the matrix supplied by the function "makeCacheMatrix"
## It is assumed that the matrix is always invertible
##

cacheSolve <- function(x, ...) {
    # collect the inverse of the matrix
    xInv <- x$getInv()

    # verify IF the inverse matrix collected above exists (cached) and is not NULL
    if(!is.null(xInv)) {                # if the inverse was cached -
        message("getting cached data")  # print a message -
        return(xInv)                    # and exit the function here.
    }

    # ELSE the object "data" is filled with the elements of the matrix and -
    # the function "solve" calculates its inverse
    data <- x$get()          # deposit the elements of the matrix in "data";
    xInv <- solve(data, ...) # calculate the inverse of the matrix;
    x$setInv(xInv)           # cache the new value of the matrix inverse;
    xInv                     # return the matrix inverse.
}
