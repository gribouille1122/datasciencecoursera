##there are two functions below.  They work together.
##Function "makeCacheMatrix()" creates an object.  It takes a matrix as its argument.
##e.g.: a<-makeCacheMatrix(matrix(1:4,2,2))
##the object stores the matrix, and also stores its inverse.
##The inverse is calculated by the second function, "cachesolve()"
##e.g.: cachesolve(a)
##cachesolve() checks to see if the inverse has been stored in object "a"
##if it hasn't, cachesolve() calculates the inverse and returns it.
##if it has, cachesolve() retrieves the inverse from memory, and returns it.
##Cachesolve also returns a message to that effect.

##makeCacheMatrix has four methods: set(), get(), setInverse(), and getInverse()
## these are called in terms of the object
## e.g.: a$get(), a$set(), a$getInverse(), a$setInverse()

makeCacheMatrix<-function(x=matrix()){
  i<-NULL ##i is the inverse, and is intialized to NULL
  set<-function(y){ 
    x<<-y
    i<<-NULL  ## i is re-set to NULL when set() is called.
  }
  get<-function()x 
  setInverse<-function(inverse)i<<-inverse 
  getInverse<-function()i 
  list(get=get,set=set,setInverse=setInverse,getInverse=getInverse)
}

##cachesolve() attempts to retrieve the inverse matrix stored in the object
##if it hasn't been stored there, it calculates it and stores it there
cachesolve<-function(x,...){
  i<-x$getInverse() 
  if(!is.null(i)){
    message("Getting cached data")
    return(i) 
  }
  i<-solve(x$get()) 
  x$setInverse(i) 
  i
}
