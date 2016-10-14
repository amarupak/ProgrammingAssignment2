## These functions perform the computation of the inversing the matrix
## however the code would be similar to the make vector because we are 
## replacing the men of the vector with the inverse of the matrix
## Chace the vector if already present and store it in a variable so that
## it can be retrieved insted of calculating 
makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL  
  set <- function(y) {        
    x <<- y        
    inv <<- NULL  
  }  
  get <- function() x  
    setinverse <- function(inverse)  inv <<- inverse  
    getinverse <- function() inv  
    list(set = set, get = get,       
         setinverse = setinverse,       
         getinverse = getinverse)
  }
      
## Cashesolve performs inverse of the matrix that is generated or cached 
## by the makeCachMatrix
cacheSolve <- function(x, ...) {        
            ## Return a matrix that is the inverse of 'x'  
  inv <- x$getinverse()  
  if(!is.null(inv)) {          
    message("getting cached data")          
    return(inv)  
  }    
  matrix <- x$get()  
  inv <- solve(matrix, ...)  
  x$setinverse(inv)  inv
}
