## Two functions. One that creates a matrix object that can cache it's inverse
## A second function that take a matrix object as input and either computes and stores the inverse
## or returns the cached value

## Creates a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {

 
  # Set inverse matrix to NULL
  i <- NULL
  # Create a function that assigns y to x global
  # and sets i to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # function that returns x
  get <- function() x
  # function that sets i as mean of x
  setinv <- function(inverse) i <<- inverse
  # function to return the mean, i
  getinv <- function() i
  # return all functions as list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function returns the inverse of a makeCacheMatrix object
## It will return a cached value if it exists

cacheSolve <- function(x, ...) {

 # call function getinv in object x, assign to i, if cached
  i <- x$getinv()
  # if i is found 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # if i is not found
  # assign x to data
  data <- x$get()
  # create inverse, assign to i
  i <- solve(data, ...)
  # store inverse in x via function from x
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
        
}
