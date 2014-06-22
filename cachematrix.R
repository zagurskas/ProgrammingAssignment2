# MakeCacheMatrix creates a special matrix object. 
# cacheSolve gives inverse of the matrix.
# If the matrix inverse has been calculated, cacheSolve 
# will return the result from cache, instead of calculation.

# 1. set the value of the matrix (11-15)
# 2. get the value of the matrix (16)
# 3. set the inverse of the matrix (17-18)
# 4. get the inverse of the matrix (19)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# 1. If inverse is calculated, then return it (30-33)
# 2. If inverse is not calculated, then calculate it (34-35)
# 3. Cache the inverse (36)
# 4. Return inverse (37)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        else data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}