## These functions implement cached matrix solving

## The function add cached getters and setters in matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y = matrix()) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setSolved <- function(solvedMatrix) m <<- solvedMatrix
    getSolved <- function() m
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## The function solves matrix or gets result from cache if exists 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolved()
    if(!is.na(m[1,1])){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setSolved(m)
    m
}
