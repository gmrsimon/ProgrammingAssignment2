## As specified in the programming assignment 2 and thanks to the sample given
## Please find here two functions based upon a specific <<- assignment operator
## that use parent environment variables
## no real big changes with the samples given, except that "mean" had been replaced
## by "solve" and the "m" variable had been replaced by "inv" for clarity


## This function returns a matrix with cache capabilities for its inverse
## (lazy initialization: first time the inverse is asked, the solve function
## is called... next times this inverse will be asked, it will be taken from the 
## cache without consumming re-calculation)
## samples
## mat2 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
## mat3 <- makeCacheMatrix(matrix(c(1,2,3,6,5,4,7,9,8), nrow=3))

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns the inverse of the matrix 
## First time, a call to solve is made with the inverse result
## cached in memory before beeing returned back
## If this function is after called again for this matrix,
## a "getting cached data" message displayed  before the 
## cached value returned without any calculation 
## samples (to be executed at least twice)
## inv2 <- cacheSolve(mat2) or print(cacheSolve(mat2))
## inv3 <- cacheSolve(mat3)or print(cacheSolve(mat3))

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
