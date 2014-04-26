## makeCacheMatrix creates a cached matrix object associated with these four set/get functions:
  ## set() creates the matrix
  ## get() retrieves the stored/cached matrix
  ## setInverse() calculates/creates the inverse matrix
  ## getInverse() retrieves the stored/cached inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) inv <<- inv
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() uses the cached matrix (produced by makeCacheMatrix()) to return either a calculated inverse (via the setInversen() function) or a cached inverse (via the getInverse() function)


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("assigning cached inverse matrix")
    return(inv)
  }
  message("calculating and assigning inverse matrix")
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}




##Testing a 2x2 Matrix Inversion
  ## See http://www.mathsisfun.com/algebra/matrix-inverse.html for calculation confirmation

    ## mtx <- makeCacheMatrix(matrix(c(4, 2, 7, 6), ncol=2))
    ## mtx$get()
    ##      [,1] [,2]
    ## [1,]    4    7
    ## [2,]    2    6

    ## inv <- cacheSolve(mtx)
    ## inv
    ##           [,1]        [,2]
    ## [1,] -0.06666667  0.26666667
    ## [2,]  0.26666667 -0.06666667


##Testing another 2x2 Matrix Inversion
  ## See http://www.mathwords.com/i/inverse_of_a_matrix.htm for calculation confirmation

    ## mtx <- makeCacheMatrix(matrix(c(4, 3, 3, 2), ncol=2))
    ## mtx$get()
    ##      [,1] [,2]
    ## [1,]    4    3
    ## [2,]    3    2

    ## inv <- cacheSolve(mtx)
    ## inv
    ##     [,1] [,2]
    ## [1,]  -2    3
    ## [2,]  3   -4


##Testing a 3x3 Matrix Inversion
  ## See http://www.mathwords.com/i/inverse_of_a_matrix.htm for calculation confirmation

    ## matrix <- makeCacheMatrix(matrix(c(1, 0, 1, 2, 4, 0, 3, 5, 6), ncol=3))
    ## matrix$get()
    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## [2,]    0    4    5
    ## [3,]    1    0    6

    ## inverse <- cacheSolve(matrix)
    ## inverse
    ##            [,1]        [,2]        [,3]
    ## [1,]  1.0909091 -0.54545455 -0.09090909
    ## [2,]  0.2272727  0.13636364 -0.22727273
    ## [3,] -0.1818182  0.09090909  0.18181818
