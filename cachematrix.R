## These pair of functions will take a matrix as input and store its inverse
## Whenever the inverse is needed, it will be computed only if it has not been
## computed before, else it will be fetced from cache

## This funciton will store the matrix and define the methods for caching

makeCacheMatrix <- function(x = matrix()) {#function takes a numeric matrix as input
     i <- NULL            #initializes the inverse of the matrix as null value
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x #use the get method to store the value of x
     setinv <- function(solve) i <<- solve #use the solve function to calculate the inverse of the matrix
     getinv <- function() i #get the inverse of the matrix when the cacheinv function is called
     list(set = set, get = get, #defines a list ot of variables to call the methods when cacheinv is called
          setinv = setinv,
          getinv = getinv)
}


## This function will fetch the inverse of the matrix from cache, if available
## else it will compute the inverse

cacheSolve <- function(x, ...) {
     i <- x$getinv() #get the inverse of the matrix from the pervious function
     if(!is.null(i)) { #check if inverse has already been calculated
          message("getting cached data") #if already calculated, get the cached values
          return(i)
     }
     data <- x$get() #if inverse is not already calculated, get the matrix
     i <- solve(data, ...) #calculate the inverse of the matrix
     x$setinv(i) #set the value of the inverse of the matrix
     i
}
