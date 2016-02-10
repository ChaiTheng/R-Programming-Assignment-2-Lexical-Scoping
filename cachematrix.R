## Programming Assignment 2 - Lexical Scoping  
## Assignment 2: Caching the Inverse of a Matrix

##---------------------------------------------------
##  Example -- during 1st run without cache involve
##---------------------------------------------------
  
##Start the Clock
## ptm <- proc.time() 
## x <- matrix(1:4, nrow = 2, ncol = 2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## Stop the Clock
## proc.time()-ptm
##  user  system elapsed      
##  0.00    0.03    0.03 
   
##---------------------------------------------------
##  Example -- during 2nd run with cache invovle
##---------------------------------------------------
# #Start the Clock
## ptm <- proc.time()
## cacheSolve(y)
##  inverse is cached
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## Stop the Clock
## proc.time()-ptm
##  user  system elapsed 
##  0       0       0 
   
## A good habit for creating cache is to reduce coslty computation when large/complex data involve. 
## Caching reduce computation time. 

makeCacheMatrix <- function(x = matrix()) {
  # Cached inverse of matrix inv is a temporary matrix
  inv <- NULL
  
  # Function of set from matrix
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  # Function of get from matrix	
  get <- function() x
  
  ## Getting and setting for inverse matrix 
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
 
  ## Return list of functions for matrix
  list(get=get, 
       set=set, 
       get_inv=get_inv, 
       set_inv=set_inv)
}
 
## cacheSolve function will retrieve the message "inverse is cached"
## indicate faster computation if re-execute the same matrix. 

cacheSolve <- function(x, ...) {

  # Get the inverted matrix 'x'
   inv <- x$get_inv()
  
  # Check for available cached 
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # Compute inverse of matrix 
  m  <- x$get()
  inv<- solve(m, ...)
  
  # Cache inverse
  x$set_inv(inv)
  
  # Return inverse of matrix
  return(inv)
}