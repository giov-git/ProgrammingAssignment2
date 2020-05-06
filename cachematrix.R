##The scripts includes 2 functions. 
## makeCachematrix takes a matrix as argument. Then it initialise the variable inverse equal to null. 
## the set function takes y as an argument and assign it the element x in the parent environment (the matrix). Inverse (in the parent environment) is confirmed as null
## the get function retrieves the matrix defined in the parent environment
## the function setinverse takes the argument solve and assign it to the object inverse in the parent environment
## the function getinverse retrieves the value of inverse
## the it creates a list when it assigns a name to the 4 previous functions

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {x}
  
  setinverse <- function(solve) {
    inverse <<- solve }
  
  getinverse <- function() {
    inverse}
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cachemean must retrieve the function solve of the matrix
## it puts inverse equal to the element defined in makeCacheMatrix's list
## it controls if it's equal to null. If it's the case it calculates solve, otherwise it retrieves the value

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

##Now i check if it's working
matrix = makeCacheMatrix(matrix(c(15,20,25,30), nrow = 2, ncol = 2))
matrix$get()
matrix$set()
cacheSolve(matrix)

      