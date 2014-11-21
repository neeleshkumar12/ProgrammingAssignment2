## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


## This function is used to get and set the matrix as well as
## get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv<- NULL 
  set <- function( matrix ) { 
    m <<- matrix 
    inv <<- NULL 
  } 
  
  get <- function() m
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv 

  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## This function retrun the inverse if it is already set. else
## it Compute the inverse using the matrix manipulation and at
## last it return the matrix. 


cacheSolve <- function(x, ...) {
  
  m <- x$getInverse() 
   
  if( !is.null(m) ) { 
    message("getting cached data") 
    return(m) 
  } 
  
  data <- x$get() 
  m <- solve(data)

  x$setInverse(m) 
  m 
}
