
## The following is two functions, one is makeCacheMatrix, and the other is cacheSolve. 

## The first function, "makeCacheMatrix", creates a special "matrix" that can cache its inverse.
## The second function, "cacheSolve", computes the inverse of the special "matrix" returned by  
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. This will save time.

## A short comment describing the "makeCacheMatrix" function 
## The first function, makeCacheMatrix, takes an argument "x" of matrix, and 
## it returns a special list with 4 list items (they are actually 4 functions wrapped in a list).
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

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


## A short comment describing the "cacheSolve" function
## The following function calculates the inverted matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverted matrix has already been calculated. If so, it gets the matrix 
## from the cache and skips the computation. Otherwise, it calculates the inverted matrix of the data and sets 
## the value of the matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m         
}

