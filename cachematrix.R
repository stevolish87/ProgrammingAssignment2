# This file contains two functions which can cache the inverse of a matrix

# First function (makeCacheMatrix) can make a special matrix which
# contains the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x <- x
  
  #Initialize the inverse of x (m) to NULL
  
  m<-NULL
  
  #Find the number of rows and columns.  If it is not a square matrix, stop the function.
  
  Coln <<- ncol(x)
  Rown <<- nrow(x)
  
  if (Rown != Coln) {
    stop("Not a square matrix, therefore no inverse is possible.")
  }
    
  # Sets the x matrix with the argument passed in as y and
  # sets the inverse (m) as a matrix of a NULL
  set<-function(y = matrix()){
    x<<-y
    m<<-matrix()
  }
  
  # Gets the original matrix
  get<-function() {
    x
  }
  
  # Sets inverse of the matrix with argument passed in as InverseMatrix
  setmatrix<-function(InverseMatrix) {
      m <<- InverseMatrix
  }
  
  # 
  getmatrix<-function() {
      m
  }
    
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
  }
  
# Will try to get the cached value of the inverse of the matrix
# If none is found, it will set the inverse

cacheSolve <- function(x=matrix(), ...) {
  
  # Return a matrix that is the inverse of 'x'
  
  # Gets the inverse of x
  
  m<-x$getmatrix()
  
  # Check to see if the inverse is NULL
  # If it isn't NULL, return the inverse of x retrieved above and end function.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # If this is reached, the inverse was NULL, so calculate and set the inverse
  data<-x$get()
  m<-solve(data, ...)
  x$setmatrix(m)
  
  # Return the inverse of x
  m
}
