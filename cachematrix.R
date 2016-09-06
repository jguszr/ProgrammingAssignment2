## Week3 assigniment by Jose Gustavo Z. Rosa (jguszr@gmail.com)
## those two functions are based on the matrix inversion with cache sutf

## Creates the object that actually contains the matrix, Pretty much follow the vector example.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setinverse <- function(solve)
    m <<- solve
  getinverse <- function()
    m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## uses the lexical scope R feature to acctually stores in memorey the inverse of the matrix passed as the x argument.
## I just follow the vector example as well.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (is.null(m)) {
    data <- x$get()
    m <- solve(data) %*% data
    x$setinverse(m)
    return(m)
  }
  message("There is some cached data hanging here...")
  m
  
  
}
