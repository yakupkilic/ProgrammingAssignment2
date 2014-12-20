## The function makeCacheMatrix creates a special vector, which is a list containing

## 1- set=set the value of the vector
## 2- get=get the value of the vector
## 3- setinverse=set the value of the inverse of the matrix
## 4- getinverse=get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
              x <<- y
              m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve function gets as an input the list, created by makeCacheMatrix function.
## cacheSolve function computes the inverse of matrix. If the inverse of the matrix is already computed, then
## the function returns the matrix inverse already from the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
          if(!is.null(m)) {
                message("getting cached data")
                return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}

