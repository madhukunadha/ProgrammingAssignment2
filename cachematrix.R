########################################################################################
## Matrix inversion is usually a costly computation and there may be some benefit to  ##
## caching the inverse of a matrix rather than compute it repeatedly.                 ##
## We are going to use 2 functions to create a matrix object that cashes its inverse  ##
## and computes the matrix object invers either from cache or from scratch.           ##
########################################################################################



##############################################################################
# makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
##############################################################################


makeCacheMatrix <- function(x = matrix())  	# Function to create the special matrix
{
  inv <- NULL																# Assign null value to a variable called "inv"
  set <- function(y) 												# The value of the matrix is set/calcualed
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x												# The value of the matrix is assigned to "get"
  setinverse <- function(inverse) inv <<- inverse		# Inverse of the matrix "x" is set/calculated here
  getinverse <- function() inv							# Inverse of the matrix is then assigned to "getinverse"
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)	 
}


#########################################################################################
# cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache. This function assumes
# that the matrix is always invertible.
#########################################################################################


cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()   # Checks for inverse in the global environment and if found "inv" is assigned the inverse
  if(!is.null(inv))   # If inverse found
  {
    message("getting cached data.")   # it displays "getting cached data."
    return(inv)                       # Prints the matrix invers and skips the remaining function
  }
  data <- x$get()                     # since existing inverse not found, gets the matrix "x" in to "data"
  inv <- solve(data)                  # calculates the inverse and assins to "inv"
  x$setinverse(inv)						
  inv                                 # Prints the inverse
}
