
##This function creates a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##The following function calculates the inverse of the matrix created with the above 
##function. However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
##in the cache via the setsolve function.

cacheSolve <- function(x, ...) {

m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



#mymat=diag(1:5) #creating a diagonal matrix for testing
#mymat
#checkmakematrix=makeCacheMatrix(mymat) 
##What we want to check is if the makeCacheMatrix 
##function is able to get cached data if same things has occured before
##It's taking a vector and decomposes the vector into a list of four functions. 
##Doing so helps R allocate computing memory more efficiently for future computations 
##operated on the same vector.

##If the function works correctly, you should be able to pass the special matrix 
##(made by the function makeCacheMatrix) to cachematrix the first time without 
##any messages( i.e. this computation is totally new, R never allocated memory to do 
##this before)
#cacheSolve(checkmakematrix) 

##When you run it the second time, the cachematrix function will pop the "getting cached data" message. I think of this as R searched through the global environment and found the setmeans and getmeans and all that, so what it does is pretty much printing what was stored previously 
#cacheSolve(checkmakematrix)

##And you and see the same thing over and over again if you run the above line more than once...
#cacheSolve(checkmakematrix)
#cacheSolve(checkmakematrix)
#cacheSolve(checkmakematrix)

######until you clear your setsolve function assignment in makematrix, then the function cachematrix runs without the message again:

#checkmakematrix$setsolve(NULL)

#cacheSolve(checkmakematrix)
