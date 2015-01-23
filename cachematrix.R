## Coursera: Data Science: Programming in R
## Assignment 2
## mcleary8

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This assignment assumes the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolution <- function(solve) m <<-solve
   getsolution <- function() m
   list( set=set, 
         get=get,
         setsolution = setsolution, 
         getsolution = getsolution )
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the 
## cache via the setsolution function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getsolution()
   if (!is.null(m)) {
      message("Getting cached data")
      retun(m)
   }
   message("Not cached, calculating using solve()")
   rawdata <- x$get()
   m <- solve(rawdata)
   x$setsolution(m)
   m
}
