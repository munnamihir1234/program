makecMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setv <- function(verse) v <<- verse
  getv <- function() v
  list(set = set, get = get, setv = setv, getv = getv)
}
cSolve <- function(x, ...) {
  v <- x$getv()
  if(!is.null(v)) {
    message("getting cd result")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setv(v)
  v
}
