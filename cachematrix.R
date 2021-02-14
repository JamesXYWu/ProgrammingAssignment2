## Following two functions are used to solve a given invertible matrix's inverse, the first function is to cache 
## the input matrix and the inversed matrix, the second function is to get the cached inversed matrix if it exists, or solve 
## from the input matrix

## makeCacheMatrix: create a list to cache input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y){
    x <<-y
    invs <- NULL
  }
  get <- function()x
  setinverse <- function(inverse) invs <<- inverse
  
  getinverse <- function() invs
  
  list(set = set, get= get, setinverse= setinverse, getinverse=getinverse)
}

## cacheSolve: get the inverse matrix from cache or solve it from input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #library(matlib)
  invs <- x$getinverse()
  
  if(!is.null(invs)){
    message("getting cached inverse matrix")
    return(invs)
  }
  mtx <- x$get()
  #invs <-Inverse(mtx)
  invs <- solve(mtx, diag(dim(mtx)[1]))
  
  x$setinverse(invs)
  invs
}
