## makeCacheMatrix function creates a special matrix and returnss list of four functions set, get, setinv and getinv
## set the value of the matrix with the function names set
## get the value of he matric with the function get
## set the value of the inverse matrix with function setinv
## get the value of the inverse matrix with function getinv

makeCacheMatrix <- function(mat = matrix()) {
        m <- NULL
        set <- function(y) {
                mat <<- y
                m <<- NULL
        }
        get <- function() mat
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## function cacheSolve checks x matrix is not null if it is not then retrieves the cached value of inverse matrix and returns the value
## else calculates the invese of the matrix i.e. do the calculation and then retunrs the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
                
}


