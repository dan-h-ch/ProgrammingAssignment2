## makeCacheMatrix - Creates a special "matrix" object that can cache it's inverse
## cacheSolve - compute inverse of makeCascheMatrix or call cache is already stored

## This is all caching.
## At start, take matrix x, clear m (will be defined later)
## $set allows you to reset without restarting
## $get calls up cached x
## $setInv and $getInv only possible is 
## output is list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function () m
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

## Solves (get inverse) of matrix a
## Requires input like z <- makeCacheMatrix(a); cannot directly use a
## Checks first is there is cached solution
## If so, returns cache regardless if matrix worked on is changed
## (notice makeCasheMatrix clears solutions cache)

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached inv")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m ## Return a matrix that is the inverse of 'x'
}
