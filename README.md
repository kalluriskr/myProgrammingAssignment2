# myProgrammingAssignment2


makeCacheMatrix <- function(A = matrix())
{
  S <- NULL
  set <- function(B)
  {
    A <<- B
    S <<- NULL
  }
  get <- function() A
  setinv <- function(B) S <<- B
  getinv <- function() S
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(A, ...)
{
  S <- A$getinv()
  if (is.null(S)) {
    message('Computing inverse...')
    data <- A$get()
    S <- solve(data, ...)
    A$setinv(S)
  } else {
    message('Returning cached inverse...')
  }
  return(S)
}
