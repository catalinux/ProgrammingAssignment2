## Put comments here that give an overall description of what your
## functions do


##usage of these functions
##Example:
##B = matrix( c(2, 1, 5, 7), nrow=2, ncol=2)
##smartMatrix<-makeCacheMatrix(A)
##calculates inverse
##cacheSolve(smartMatrix)
##
## Down at the bottom file we have a worinking code that uses those two functions.



## builds a special list that stores the matrix, its inverse and 2 methods of
## setting and retrieving the cahed result of solve method (inverse)

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(as.matrix(x))){
         stop("1st argument should be a matrix")
  }
  
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the cache result of `solve` function if there is one set on the smart matrix "object"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInverse<-x$getinverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data)
  x$setinverse(mInverse)
  mInverse
}


#example


print("Fisrt matrix")
A <- matrix( c(1, 1, 5, 7), nrow=2, ncol=2)
smartMatrix<-makeCacheMatrix(A)
##calculates inverse
cacheSolve(smartMatrix)
##does not calculate, it uses cached data
cacheSolve(smartMatrix)


print("Second matrix")
C <- matrix( c(3, 1, 5, 7), nrow=2, ncol=2)
smartMatrix<-makeCacheMatrix(C)
##calculates inverse
cacheSolve(smartMatrix)
##does not calculate, it uses cached data
cacheSolve(smartMatrix)
