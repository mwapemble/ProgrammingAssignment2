## This is an answer to the 2nd Programming Assignment for the R Programming course
## Lexical Scoping
## It uses as a base the example functions provided in the assignment
## But modifies them slightly to perform matrix operations - inversion, cache 
## & storage, rather than taking the mean of a vector.

## Takes a matrix, x, and sets it in to the parent environment as a list of functions
## set, get, setinverse & getinverse
## Initially, the inverse is set to NULL

makeCacheMatrix <- function(x = matrix()) {
  ## Initialise variable
  i <- NULL
  
  ## set sub.function sets value into parent environment variable x and NULL in to parent environment variable y
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function operates on a list object created by makeCacheMatrix
## It checks for a cached inverse and returns it if it exists.
## Otherwise it calculates the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Fetch the possibly NULL inverse object from the supplied list.
  i <- x$getinverse()
  
  ## Test to see if cached inverse is not NULL
  if(!is.null(i)) {
    ## Return cache message then cached answer
    message("getting cached data")
    i
  }
  else
  {
    ## Fetch matrix from list
    data <- x$get()
    ## Solve matrix using standard function
    i <- solve(data)
    ## Set inverse in to list
    x$setinverse(i)
    ## return answer
    i
  }  
}
