## The following functions calculate the inverse of a matrix and saves it to the Global Environments memory cache.
## This allows the saved value to be retrieved instead of recalculating the inverse in the Local Environment memory cache.

## The makeCacheMatrix function creates a list object. It provides the following functionality:

## Set the value of the matrix 
## Get the value of the matrix 
## Set the value of the inverse 
## Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  ## create the matrix object x and associated sub-functions/methods 
  
  ## define the cache as m 
  m <- NULL 
  set <- function(y) { 
    x <<- y           ## This will assign the input matrix y to the variable x in the parent environment 
    m <<- NULL        ## This will re-initialize m in the parent environment to null 
  } 
  get <- function() x ## This will return the matrix x 
  setinverse <- function(inverse) m <<- inverse ## This sets the cache m equal to the inverse of the matrix x 
  getinverse <- function() m                    ## This will return the cached inverse of x 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
}


## The following function calculates the inverse of the list object created above
## It checks to see if the inverse has already been calculated. 
## If it is available, it 'get's the inverse from the Global Environment cache and skips the computation. 
## Else, it calculates the matrix inverse and sets the value of the inverse in the cache via the 'setinverse' function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinverse(m) 
  m 
}
