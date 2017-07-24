## Put comments here that give an overall description of what your
## functions do

## The first function "makeCacheMatrix" creates an object that stores matrix
## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. Set the value of Inverse matrix
## 4. Get the value of Inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
	i <- NULL
        
	
	setmatrix <- function(y) 
	{
         
	       x <<- y

               m <<- NULL
        
	}

        
	getmatrix <- function() x
        
	
	setinverse <- function(inverse) i <<- inverse 
        
	
	getinverse <- function() i
        
	
	list(setmatrix = setmatrix, 
	getmatrix = getmatrix,
 
	setinverse = setinverse,
 
	getinverse = getinverse)


}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()

  if (!is.null(i)) 
  {
          message("getting cached data")
          return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
