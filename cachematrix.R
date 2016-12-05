## The functions- makeCacheMatrix and cacheSolve- described here create a special matrix
## that can cache its inverse and retrieve the inverse from the cache if the matrix has not changed


## The function- makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the variable cachemi that stores cache value
  cachemi <- NULL
  
  # Create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cachemi <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Compute the inverse of the matrix (assume supplied matrix is always 
  # invertible) and store in cache
  setMatrix <- function(solve) cachemi <<- solve
  
  # Get the inverted matrix from cache
  getInverse <- function() cachemi
  
  # Passe the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## The function- cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return inverse of matrix x
  cachemi <- x$getInverse()
  
  # Return inverted matrix from cache if it exists
  # othewise create the matrix in working environment
  
  if (!is.null(cachemi)) {
    message("Getting cached data- matrix inverse")
    
    # Display cached value in console
    return(cachemi)   
  }

  # Create matrix 
  data <- x$get()   
  
  # Set and return inverse of matrix- assume supplied matrix is invertible
  cachemi <- solve(data, ...)

  # Set inverted matrix in cache
  x$setMatrix(cachemi)
  
  # Display matrix in console
  return (cachemi)
   
}

