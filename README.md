# ProgrammingAssignment2-master
Lizbeth's Answer



makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}






makeCacheMatrix <- function(x = matrix()) { 
  
  matrixinverse <- NULL                     
  
  set <- function(y) {                      
    x <<- y
    
    matrixinverse <<- NULL              
  }

  get <- function() x                           
  
  setinverse <- function(solve) matrixinverse <<- solve 
  e     
  getinverse <- function() matrixinverse        
         
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}






source("ProgrammingAssignment2-master")
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()dir()
cacheSolve(my_matrix)


cacheSolve(my_matrix)


my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()



my_matrix$getInverse()

cacheSolve(my_matrix)



cacheSolve(my_matrix)

my_matrix$getInverse()




