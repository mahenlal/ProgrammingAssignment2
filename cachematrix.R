## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCachedMatrix creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the Inverse Matrix
#get the value of the Inverse Matrix
#########################################
makeCacheMatrix <- function(x = matrix()) {
    inv_x<-NULL
    set<-function(y){
      x<<-y
      inv_x<<-NULL
    }
    get <- function() x
    setInv<-function(solve) inv_x <<-solve
    getInv<-function() inv_x
    list(set = set , get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
#The following function calculates the Inverse of the special "vector" 
#created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets 
#the value of the inverse in the cache via the setInv function.
#################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x<-x$getInv()
  if (!is.null(inv_x)){ 
        message ("getting Cached Data")
        return(inv_x)    
    }
  data<-x$get()
  inv_x<-solve(data)
  x$setInv(inv_x)
  inv_x
  
}
