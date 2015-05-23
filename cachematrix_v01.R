## Function: `makeCacheMatrix`
## This function creates a special"matrix" object that cache its inverse
## This function is a list of four functions:
##    1. "setMat" - set the elements of the matrix
##    2. "getMat" - get the elements of the matrix
##    3. "setInv" - set the inverse of the matrix
##    4. "getInv" - get the inverse of the matrix

makeCacheMatrix <- function(X = matrix()) {
        X.Inv <- NULL

    # 1.
    setMat <- function(Y) { # changes the matrix stored in the main function
        X     <<- Y         # assign the value of the new matrix Y to X
        X.Inv <<- NULL      # restores to NULL the value of the old matrix inverse X.Inv
    }
    # 2.
    getMat <- function() { # return the matrix stored in the main function
        X
    }
    # 3.
    setInv <- function(inv) { # store the matrix inverse in the main function
        X.Inv <<- inv
    }
    # 4.
    getInv <- function() { # return the matrix inverse stored in the main function
        X.Inv
    }

    # return the matrix type list of functions
    list(setMat = setMat,
         getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}

##########

## Function: `cacheSolve`
## This function computes the inverse of the special "matrix" supplied by the function `^\makeCacheMatrix`
## It is assumed that the matrix is always invertible
##

cacheSolve <- function(X, ...) {
    # collect the inverse of the matrix
    X.Inv <- X$getInv()

    # verify IF the inverse matrix collected above exists (cached) and is not NULL
    if(!is.null(X.Inv)) {                 # if the inverse was cached -
        message("getting cached data")    # print a message -
        return(X.Inv)                     # and exit the function here.
    }

    # ELSE the object "data" is filled with the elements of the matrix and
    # the function `solve` calculates its inverse
    data <- X$getMat()        # deposit the elements of the matrix in "data";
    X.Inv <- solve(data, ...) # calculate the inverse of the matrix;
    X$setInv(X.Inv)           # cache the new value of the matrix inverse;
    X.Inv                     # return the matrix inverse.
}
