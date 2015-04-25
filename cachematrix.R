## Finds the inverse of a matrix

## creates a list of functions that can be used to cache 
## the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(mat)
  {
    x <<- mat
    inv <<- NULL
  }
  
  get <- function() x
  getInv <- function() inv
  
  setInv <- function(invParm) inv <<- invParm
  
  list(set = set,get = get,
       setInv = setInv,getInv = getInv)
}


## Checks if the Inverse is already present in cache or not
## if already cached then fetch from cache
## else calculate inverse and cache it.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  ## checking if inverse is already cached
  inv <- x$getInv()
  if(!is.null(inv))
  {
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()
  
  ## finding and caching inverse
  inv <- solve(mat)
  x$setInv(inv)
  inv
}
