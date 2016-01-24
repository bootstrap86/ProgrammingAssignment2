# Programming Assignment 2

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Example run:

# > x = rbind(c(1, 2), c(3, 4))
# > mtx = makeCacheMatrix(x)
# > mtx$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 
# # No cache in the first run
# > cacheSolve(mtx)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 
# # Retrieving from the cache in the second run
# > cacheSolve(mtx)
# getting cached data.
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > 

#