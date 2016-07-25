## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##MCreats matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
                
        }
        #gets the matrix not the inverse
        get <- function()x
        
        #set the inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set,get=get, 
             setinverse = setinverse,
             getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Getting cached matrix")
                return(inv)
        }
        data <-x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}


