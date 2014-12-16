# this function works like a class, it creates a list
# and it uses <<- assignment operator

  makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL # this is where the result of inversion is stored
      # A setter function, use this to set a matrix to object created by makeCacheMatrix function
     
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL
      }

      get <- function() x 
      setInv <- function(inv) xinv <<- inv 
      getInv <- function() xinv # return the inversed matrix
      # return a list that contains these functions, so that we can use
      # makeCacheMatrix object like these
      # x <- makeCacheMatrix(testmatrix)
      # x$set(newmatrix) # to change matrix
      # x$get # to get the setted matrix
      # x$setInv # to set the inversed matrix
      # x$getInv # to get the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }

  cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
         if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

  test <- matrix(runif(9,1,100),3,3)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)

  testInv <- cacheSolve(testCached)
