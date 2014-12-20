## Invoke makeCacheMatrix to create a cache
## wrapper for the matrix, mx
## Then call cacheSolve(mx) to solve(mx) to
## compute the transpose. For all future calls
## to cacheSolve(mx) the precomputed inverse
## value will be returned.

## Return a list that stores the precomputed inverse values
## of a matrix, mx
makeCacheMatrix <- function(mx = matrix()) {
  inverse <- NULL
  
  # Set the matrix to m
  set <- function(m) {
    mx <<- m
    inverse <<- NULL
  }
  
  # Return the original matrix
  get <- function() mx
  
  # Set the inverse value of matrix, mx
  setinverse <- function(i) inverse <<- i
  
  # Return the inverse value of, mx
  getinverse <- function() inverse
  
  # Create a cache structure for matrix mx with
  # getters and setters for the original matrix
  # and its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x
## 
## cacheSolve first checks the cache to see if the inverse of
## matrix, x has already been computed and is stored in the cache.
## If the cached value is NULL,  the solve function is called
## to compute the inverse which is then stored in the cache

cacheSolve <- function(x, ...) {
  
  # Try to retrive the precomputed inverse value of matrix, x$mx
  inverse <- x$getinverse()
  
  # If the inverse is not NULL, the it has been precomputed
  # Return the inverse value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Otherwise fetch the matrix, mx from makeCacheMatrix
  data <- x$get()
  # Compute the inverse
  inverse <- solve(data)
  # Store the inverse in the cache
  x$setinverse(inverse)
  # Return the computed invere value
  inverse
}

