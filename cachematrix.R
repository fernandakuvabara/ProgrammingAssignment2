## The following functions create a matrix and calculate its inverse


## The first funcion it is responsible for create a "special vector" through a list with 4 functions,
##with these functions you can see the value of the matrix and its inverse and also set a new matrix and
## a new inverse
makeCacheMatrix <- function(x = numericMatrix()) {
  m <- NULL
  set <- function(y) {  ##set a new value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x   
  setinverse <- function(inverse) m <<- inverse ##here you can set a value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the function cacheSolve it is a function that uses the features of makeCacheMatrix function to 
##calculate the inverse of the matrix given in the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ##return value set for data in the makeCacheMatrix function
  }
  data <- x$get()
  m <- solve(data, ...) ##solve inverse
  x$setinverse(m)
  m
}

