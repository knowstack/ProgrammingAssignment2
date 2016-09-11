##The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y){
    x<<-y
    inversematrix <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) inversematrix <<-inverse
  getinverse <- function() inversematrix
  list(set=set, get=get,setinverse = setinverse, getinverse = getinverse)
}



#cacheSolve returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  print(inversematrix) #will print null the first time. Second time will return the cacheddata
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
}

