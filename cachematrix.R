##Cache lab, create an object, initialize a property and then when requested, check if the value has been 
##already setted, if so, then return it, if not, then calculate it, and store it as in a cache

## First create a function that will create a cacheable structure, with get and set, in order to check 
## if it has been initialized
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setsolve <- function(solve) {
    m <<- solve
  }
  
  getsolve <- function() {
    m
  }
  
  return (list(
    set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve
  ))
  
}


## cacheSolve receives a structure that can answer if it has been initialized, if so, retrieve it 
## pre-calculated value, if not, then call its 'set' funcion to initialize it value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
