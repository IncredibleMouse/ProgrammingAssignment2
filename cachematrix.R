## Neo... 
## You're searching for it. 
## Do you want to know what it is?

## Tired of waiting for your computed inverse matrix to calculate?
## The makeCacheMatrix function has the answer.
## For only 19.99, you too can store the values of that caluclation
## once, in the parent global environment, and never have to calculate it again.
## Unless, of course, you close R and don't save your environment space.

## Below you will find two functions and their example usage.
## The first manages the storage and retrieval of that inverted matrix.
## The second either computes the inverse matrix, or if it's available in cache, 
## will shoot out data like a lightning bolt from an angry cloud.

## I think this was supposed to be shorter. I better stop. You get the point.
## Frankly, I only have a superficial understanding of what's going on here. 
## I mostly just read the instructions, and started tweaking 
## for a matrix instead of a vector.  Good luck to all with the class! -mouse


## Function: makeCacheMatrix()
## This function serves to handle the getting and setting 
## of a matrix into cached variables. The <<- is an assigmment operator
## that indicates the variable names and values are stored in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## The core functions set, get, setmatrix, and getmatrix
  ## set: Using the passed in matrix (x), apply it to a new set of global variables
  ## leaving the m variable null, ready to hold the computed values.
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  ## get: return the square matrix x
  get <- function() x
  ## setmatrix: store the computed values into the global environment.
  setmatrix <- function(mat) m <<- mat
  ## getatrmix: return the computed values (inverse matrix) from the global environment.
  getmatrix <- function() m
  
  ## the list holds all of our functions, which will now be available 
  ## as functions of x in code outside of this function.
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Function: cacheSolve()
## Accepts a square Matrix, and returns an inverse Matrix
## This function utilizes the constructs made availabe by the makeCacheMatrix function
## in order to determine if the data we need is in cache.
## if this data is not in cache, the inverse matrix is computed, and stored in cache.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    ## The data was found in cache, therefore, the results are retrieved faster (not computed).
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## generate the inverse matrix, allowing for additional parameters.
  m <- solve(data, ...)
  ## Store the results of the iMatrix in cache, for faster retrieval next time.
  ## If you're wondering where setmatrix() came from, see the makeCacheMatrix function.
  x$setmatrix(m)
  m  
}

## Example Usage ------------------------------------------------------------
GenSeq <- seq(1:4)  ## generate a simple 4 digits.
GenMat = makeCacheMatrix(matrix(GenSeq, 2));  ## Pass in a square matrix for cache management.
cacheSolve(GenMat); ## You will notice the result is the computed inverse matrix.
cacheSolve(GenMat); ## Run again, you will notice the same results, but was pulled from cache.
GenMat$get();       ## A direct call to the cache of the square matrix.
GenMat$getmatrix(); ## A direct call to the cache of the computed square inverse matrix.


## Follow the white rabbit.
## ▂▃▅▇█▓▒░ MᵒυϟΣ ░▒▓█▇▅▃▂
ۜ
