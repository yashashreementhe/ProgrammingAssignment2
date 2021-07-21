makeCacheMatrix <- function(x = matrix()) # function to create a matrix & cache it's inverse
{ 
  inverse_matrix <- NULL # Setting inverse matrix to null
  setmatrix <- function(y) 
  {                    
    x <<- y                 # function to set only new value of a matrix            
    inverse_matrix <<- NULL                        
  }
  getmatrix <- function() x         # to get value of the matrix            
  
  setinverse <- function(inverse) inverse_matrix <<- inverse  # function to set the inverse value of the matrix
  getinverse <- function() inverse_matrix          # function to get the inverse matrix           
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) #function to retrieve the inverse of the matrix from cache
{
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) #message to display data being fetched from cache
  {
    message("Fetching cached data....")
    return(inverse_matrix)
  }
  data <- x$getmatrix()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}


