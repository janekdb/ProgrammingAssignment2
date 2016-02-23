#` The two functions makeCacheMatrix and cacheSolve can be used in tandem to
#` create objects which store a matrix along with a cached inverse. The inverse
#' is calculated on-demand and provided the matrix it is derived from does not
#' get replaced then the next call to get the inverse will return the cached inverse
#' which can save time especially for large matrices.
#' 
#' Caching off intermediate result values can reduce program execution time particular
#' if the calculation would otherwise be called multiple times within a loop body.
#' 
#' Example usage:
#' 
#' m <- matrix(1:4, nrow = 2)
#' c <- makeCacheMatrix(m)
#' for(in in 1:1000) {
#'   m.inv <- cacheSolve(c)
#'   <other calculations which might replace the matrix stored in c>
#' }

#' This function creates a special "matrix" object that can cache its inverse.
#'
#' @param x An invertible matrix.
#' @return A list with elements providing access to the original matrix x and
#' the cached matrix inverse if that has been set.  
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(x.new) {
    x <<- x.new
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse.new) inverse <<- inverse.new
  
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#' 
#' If the inverse has already been calculated (and the matrix has not changed), then the cached
#' matrix inverse will be returned. Otherwise the inverse will be calculated then cached and
#' returned.
#' 
#' @param x A special "matrix" object of the type returned by makeCacheMatrix.
#' @return The inverse of the matrix 'x'.
cacheSolve <- function(x, ...) {
   if(is.null(x$getinverse())) {
    x$setinverse(solve(x$get()))
  }
  x$getinverse()
}

#' This function tests the matrix inverse caching functions.
#' 
#' It tests
#' a) The returned inverse is correct.
#' b) The return inverse is correct after replacing the matrix.
#' c) The caching reduces the time it takes to get the inverse.
#' 
#' @param testmatrixsize The size of the matrix to use to test caching times
#' with. Should be large enough that the inversion time is detectable.
testCachingForMatrices <- function(testmatrixsize = 2000) {
  # Test the inverse is calculated correctly
  m <- matrix(1:4, nrow = 2)
  expected <- solve(m)
  c <- makeCacheMatrix(m)
  c.solved <- cacheSolve(c)
  #print(expected)
  #print(c.solved)
  stopifnot(c.solved == expected)
  
  # Test the inverse is recalculated correctly
  m.new <- matrix(2:5, nrow = 2)
  expected.new <- solve(m.new)
  c$set(m.new)
  c.solved <- cacheSolve(c)
  #print(expected.new)
  #print(c.solved)
  stopifnot(c.solved == expected.new)
  
  # Test caching is effective
  n <- testmatrixsize
  largematrix <- matrix(runif(n ^ 2), n)
  cl <- makeCacheMatrix(largematrix)
  
  elapsed <- numeric()
  elapsedIdx <- 3
  
  for (i in 1:4) {
    st <- system.time({
      inverse <- cacheSolve(cl)
    })
 
    inverse <- cacheSolve(cl)
    # Sanity check
    stopifnot(ncol(inverse) == testmatrixsize)
    print(paste(
      "Iteration #", i, " seconds: ", st[elapsedIdx], sep = ""
    ))
    elapsed[i] = st[elapsedIdx]
  }
  
  # Confirm all calls after the first were quicker
  stopifnot(elapsed[1] > max(elapsed[2:4]))
}
