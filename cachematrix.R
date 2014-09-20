## A pair of functions that can catch inverse of matrix

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {  
                  
              inv_a <- NULL                  ##Initialise the inverse property 
              set <- function(y) {           ##Set matrix
              x <<- y
              inv_a <<- NULL
              }
              
              get <- function ()x                                 ##Set matrix
              setinverse <- function(inverse) inv_a <<- inverse   ## Set inverse of matrix
              getinverse <- function()inv_a                       ## Get inverse of matrix
              list (set = set, get = get,                         ## Return a list of method
                    setinverse = setinverse,
                    getinverse = getinverse)
              }



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
              inv_a <- x$getInverse()         ##Returns matrix that is inverse of x
              if(!is.null(inv_a)){            ##Returns the inverse if it is already set
              message ("getting cached data")
              return(inv_a)
              }
              
              data <- x$get()                    ##Get matrix from object
              inv_a <- solve(data) %*% data      ##Calculate inverse using matrix multiplication
              x$setInverse(inv_a)                ##Set inverse to object
              inv_a                              ##Return matrix
}
  
 