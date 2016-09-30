## makeCacheMatrix()builds a set of functions
## set(), get(),setsolve() and getsolve() 
## returns the functions within a list to the parent environment.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

##cacheSolve()  is able to calculate and store the 
##inverse for the matrix used in the function makeCacheMatrix .  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$gets()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

## Example:
## > x<-matrix(c(2,1,5,3),2,2)
## > x
##        [,1] [,2]
## [1,]    2    5
## [2,]    1    3
## > myMatrix<-makeCacheMatrix(x)
## > cacheSolve(myMatrix)
##        [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
