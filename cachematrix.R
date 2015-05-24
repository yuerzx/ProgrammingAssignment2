## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #prepare for the cache variable
  m <- NULL
  #function to set the martrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #function to return the orginal matrix
  get <- function() x
  #function to set reverse matrix
  set.r.matrix <- function(r.matrix) m <<- r.matrix
  get.r.matrix <- function() m
  list(set = set, get = get,
       set.r.matrix = set.r.matrix,
       get.r.matrix = get.r.matrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$get.r.matrix()
      if(!is.null(m)) {
        message("getting cached reverse matrix")
        return(m)
      }
      #if there is no catch for the matrix 'x', then we are ready to set one. 
      data <- x$get()
      m <- solve(data, ...)
      x$set.r.matrix(m)
      m
}
