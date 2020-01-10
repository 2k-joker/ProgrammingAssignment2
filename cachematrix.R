##makeCacheMatrix caculates and caches the inverse of a square matrix.
##cacheSolve retrives the cached inverse if it exists, or finds and caches 
##the inverse if it does not.

#creates a square matrix, finds its inverse and stores in a cache location called 'i'
makeCacheMatrix <- function(x = matrix()){
  
      i <- NULL
      set <- function(y){
        x <<- y
        i <<- NULL
      }
  
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

#retives and returns the a solved matrix cached by 'makeCacheMatrix' 
#OR solves and returns the matrix inverse if it hasn't been cached yet
cacheSolve <- function(x, ...){
  
      i <- x$getinverse()
      if(!is.null(i)){
          message("getting cached inverse of your matrix")
          return(i)
      }
      
      data <- x$get()
      i <- solve(data, ...)
      x$setmean(i)
      i
  
}
