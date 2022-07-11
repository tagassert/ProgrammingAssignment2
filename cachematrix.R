## The following function creates a "Matrix".  
#  It is actually an object with four attributes.
#  set() stores the intended matrix
#  get() returns the matrix
#  setinv() caches the inverse of the matrix; we don't want to compute this 
#      more than once
#  getinv() returns the cached inverse of the matrix

makeCacheMatrix <- function(M = matrix()) {
    m_inv = NULL
    set = function(y) {
        M <<- y
        m_inv <<- NULL
    }
    get = function() M
    setinv = function(inv) m_inv <<- inv
    getinv = function() m_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function returns the inverse of the matrix object from above
#  If the inverse is already known, then the cached inverse is returned
#  If the inverse is not know, then it is computed, cached, and returned

cacheSolve <- function(x, ...) {
    m_inv = x$getinv()
    if(!is.null(m_inv)) {
        message("Retrieving cached data")
        return(m_inv)
    }
    data = x$get()
    m_inv = solve(data, ...)
    x$setinv(m_inv)
    m_inv
}
