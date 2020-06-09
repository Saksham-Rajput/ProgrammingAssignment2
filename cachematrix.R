## The following two functions are used to calculate the inverse of a matrix
## The first function has setters and getters function for matrix and inverse
## The second function calculates inverse after checking whether anything is 
## stored in cache or not

## makeCacheMatrix function is used to return an object of type makeCacheMatrix
## The object can access all components of makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(a) inv <<- a
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv )

}


## cacheSolve function takes the list returned in previous function as 
## as formal arg and returns inverse of matrix after checking cached data

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    print("Getting cached data")
    inv
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
