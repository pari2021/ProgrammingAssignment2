## Both the functions can be together to calculate the inverse of a matrix.
## If the inverse has been calculated already, a message is printed saying 
## "getting cached data" and the inverse of the matrix is returned, otherwise 
## the inverse is calculated for the new matrix and returned. A function object
## must be defined for the makeCacheMatrix function, and then passed on to the
## cacheSolve function for this to work.


## The first function makeCacheMatrix does the following:
##    1. assigns a NULL value to the variable inversedMatrix 
##    2. returns a list object of four functions namely set(), get(), 
##       setMatrix() and getMatrix(). 
## The set() function takes the values for the matrix whose inverse 
## is to be calculated.
## The get() function returns the values which are passed in the 
## makeCacheMatrix function
## The setMatrix() returns assigns matrix values which are passed in the  
## makeCacheMatrix function to the inversedMatrix variable and returns this 
## value
## The getMatrix() function returns the variable inversedMatrix

makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(matrix) inversedMatrix <<- matrix
        getMatrix <- function() inversedMatrix
        list(set = set,
             get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## The cacheSolve function first checks if the inverse has been calculated
## already. If it has the cached data is returned and no actual inverse is 
## calculated. Else, the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        inversedMatrix <- x$getMatrix()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        matrix <- x$get()
        inversedMatrix <- solve(matrix)
        x$setMatrix(inversedMatrix)
        inversedMatrix
}
