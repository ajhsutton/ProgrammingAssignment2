## Coursera R Programming
## Pogramming Assignment 2
## This file contains the solution to Programming Assignment 2
## ------------------------------------------------------------
## makeCacheMatrix creates a list of functions which:
## - set: Set the value of a matrix
## - get: Get the value of a matrix
## - setInverse: Set the inverse of a matrix
## - getInverse: Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     ## The matrix inverse
     inv <- NULL
     ## A function to set the matrix value
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     ## A function to get the matrix value
     get <- function() x
     ## A function to set the matrix Inverse
     setInverse <- function(inverse) inv <<- inverse
     ## A function to get the matrix Inverse
     getInverse <- function() inv
     ## Return a list of functions
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## ----------------------
## This function will return the inverse of a matrix "x". 
## If the inverse does not currently exist, it will be cached
## in "x" for future use.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## Check if the matrix Inverse exists 
     x_inverse <- x$getInverse()
     ## If the matrix Inverse exists, return the inverse
     if(!is.null(x_inverse)) {
          message("getting cached data")
          return(x_inverse)
     }
     ## The matrix inverse does not exist, therefore
     ## 1. Get the matrix
     x_matrix <- x$get()
     ## 2. Solve for the matrix inverse using solve(x,...)
     x_inverse <- solve(x_matrix, ...)
     ## 3. Cache the matrix inverse in x
     x$setInverse(x_inverse)
     ## Return the matrix inverse
     x_inverse
}
