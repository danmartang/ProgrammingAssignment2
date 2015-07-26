## These two functions are used to create a special "matrix" (really a list of 
## functions) and then to calculate the inverse of the matrix. In order to save
## time, once the inverse is calculated the value is cached. If we need to caculate
## the inverse again, the value will be pulled from the cache to save time/effort

## Creates a speacial "matrix" which is really a list containing 4 functions 
##to set/get the value of the matrix & set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  set <- function(y){
      x <<- y
      I<<-NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I<<-Inverse
  getInverse <- function() I
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Calculates the inverse of the special "matrix". If inverse was already
##calculated, it pulls from the cache. Otherwise calculates & saves to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      I <- x$getInverse()
      if(!is.null(I)){
            message("Getting Cached Data")
            return(I)
      }
      data <- x$get()
      I <- solve(x,...)
      x$setinverse(I)
      I
      
}
