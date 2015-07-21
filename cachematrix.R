#
# Programming world will never be the same!
#
#The function makeCacheMatrix creates a list 
#of functions from given matrix "x" to: 
#
#set the value of the matrix ($set)
#get the value of the matrix ($get)
#set the value of the inversed matrix ($setinversed)
#get the value of the inversed matrix ($getinversed)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversed <- function(solve) m <<- solve
  getinversed <- function() m
  list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}

#The function "cacheSolve" at start do a simple check to see 
#if the argument has already been prepared by "makeCacheMatrix()".
#If not then function stops with error message.
#If prepared, then function do a next check:
#if the inverse has already been done ($getinversed is not NULL
#and contains a pointer to cached data).
#If so, function returns the value of inversed matrix from the cache and 
#skips the computation with nice red message.
#Otherwise, it calculates the inverse of the matrix by function "solve()" 
#and put the inversed matrix into the cache via the "setinversed" function.

cacheSolve <- function(x, ...) {
  if (class(x) != "list") stop("Do makeCacheMatrix() first!")
  m <- x$getinversed()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversed(m)
  m
}
# Done. But it's was a really brainhack for me :(
