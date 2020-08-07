## This is a function that can create a matrix object that can cache its inverse
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cont <- NULL
  set <- function(y){
    x <<- y
    cont <<- NULL
  }
  get <- function() {x}
  setcont <- function(inverse) {cont <<- inverse}
  getcont <- function() {cont}
  list(set = set, get = get,
       setcont = setcont,
       getcont = getcont)

}

##This function computes the inverse of the matrix returned by 
##the first function

cacheSolve <- function(x, ...) {
  cont <- x$getcont()
  if(!is.null(cont)) {
    message("getting inverted data")
    return(cont)
  }
  dados <- x$get()
  cont <- solve(dados, ...)
  x$setcont(cont)
  cont
}