## Put comments here that give an overall description of what your
## functions do


##usage of these function s
##Example:
##B = matrix( c(2, 1, 5, 7), nrow=2, ncol=2)
##a<-makeCacheMatrix(B)
##calculates inverse
##result<-cacheSolve(a)
##does not calculate, it uses cached data
##result<-cacheSolve(a)



## builds a special list that stores the matrix, its inverse and 2 methods of
## setting and retrieving the cahed result of solve method (inverse)

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}



