## The following are a pair of functions which evaluate the inverse of a matrix and eliminate repetitive calculations

##Creation of special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y)
    {x<<-y
     m<<-NULL}
  get <- function() 
    {x}
  setinverse <- function(inverse) 
    {i <<- inverse}
  getinverse <- function() 
    {i}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...)
{ i <- x$getinverse()
    if(!is.null(i)) 
    {
      message("getting cached data")
      return(i)
    }
  else
  {
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i       
   }
}
