## Put comments here that give an overall description of what your
## functions do
#   Matrix inversion is a costly computational operation. Caching the inverse of a 
#   matrix instead of repeatetly compuiting it will lower the cost of the operation. The
#   following two functions are used to cretae and cache the inverse of a matrix.
#   The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#   The makeCacheMatrix function is very similar to the makeVector (almost identical)

## Write a short comment describing this function
# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#   set the value of the matrix
#   get the value of the matrix
#   set the value of inverse of the matrix
#   get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv <<- inv
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The inverse of the matrix is computed by the cacheSolve function as follows:
# Retrieve the ineverse matrix from cache
# If the return value is not NULL the inverse was already computed, 
# Therefore the inverse gets returned from the cache. 
# Clearly the first time the code runs the iverse will be computed 
# since it will not exist in cache.  Once the inverse is computed 
# it will be saved in cache using the setinverse function 
# to be retrieved the second time around
# The cacheSolve function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# Sample run
#  x = rbind(c(2,3,0),c(1,3,4), c(1,2,1))
#  m = makeCacheMatrix(x)
#  m$get()
#  [,1] [,2] [,3]
#  [1,]    2    3    0
#  [2,]    1    3    4
#  [3,]    1    2    1
#  cacheSolve(m) ## Inverse matrix is computed
#  [,1] [,2] [,3]
#  [1,]    5    3  -12
#  [2,]   -3   -2    8
#  [3,]    1    1   -3
#  cacheSolve(m) ## Inverse matrix retrieved from cache