
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL ##store the cashed inverse matrix
  set <- function(y) { ##setter for the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##getter for the matrix
  setinverse <- function(inverse) inv <<- inverse ## setter for the inverse
  getinverse <- function() inv ##getter for the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##return the matrix
}


cacheSolve <- function(x, ...) { ##returns the inverse of the matrix
  inv <- x$getinverse()
  if(!is.null(inv)) { ##of the inverse is already calculated, it is returned
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()  ##if the inverse is not calculated, we calculate it
  inv <- solve(data)
  x$setinverse(inv) ##cache the inverse
  inv ##return it
}
