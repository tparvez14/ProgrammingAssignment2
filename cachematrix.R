#Changed every reference to "mean" to "solve."
#by converting the input x to a matrix
#and setting the solved value "inver" as a null.
#The 'makeCacheMatrix' function creates a cache matrix object. 
#It takes an optional argument x which initializes the matrix with a default value of a 3x3 matrix containing random values from 2 to 200.

makeCacheMatrix <- function(x = matrix(sample(2:200, 9), 3, 3)) {
  # Initialize the matrix and inverse variables
  inver <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Assign the matrix to 'x'
    inver <<- NULL  # Clear the inverse cache
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setsolve <- function(solve) {
    inver <<- solve  # Assigned the inverse 
  }
  
  # Function to get the inverse
  getsolve <- function() inver
  
  # Return the functions as a list
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}

##'makeCacheMatrix' function can be use as follows:

# Create a cache matrix object with a default matrix
cacheMatrix <- makeCacheMatrix()

# Set a new matrix
newMatrix <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2, byrow = TRUE)
cacheMatrix$set(newMatrix)

# Get the matrix
matrix <- cacheMatrix$get()
print(matrix)
# Output:
#      [,1] [,2]
# [1,]    5    6
# [2,]    7    8

# Set the inverse
inverse <- solve(newMatrix)
cacheMatrix$setsolve(inverse)

# Get the inverse
cachedInverse <- cacheMatrix$getsolve()
print(cachedInverse)
# Output:
#      [,1] [,2]
# [1,]   -4  3.0
# [2,]    3 -2.5



## Write a short comment describing this function

#The cacheSolve function takes a cache matrix object x as input and calculates the inverse of the matrix. 
#The function first tries to retrieve the cached inverse s from the cache matrix object.
#If the inverse is found, it returns the cached inverse and displays a message stating that the cached inverse matrix is being retrieved. 
#If the cached inverse is not found, it checks if the input matrix is a valid square matrix. 
#If the input is not a square matrix, it throws an error using the stop function.


cacheSolve <- function(x, ...) {
  inver <- x$getsolve()
  
  if (!is.null(inver)) {
    message("Retrieving cached inverse matrix")
    return(inver)
  }
  
  data <- x$get()
  
  if (!is.matrix(data) || !identical(nrow(data), ncol(data))) {
    stop("Input is not a square matrix")
  }
  
  inverse <- solve(data, ...)
  x$setsolve(inver)
  
  message("Calculating inverse matrix")
  inver
}
