# Autor: Jorge R. Martinez
# Date: 9/20/14 
# Title: R Programming - Lexical Scoping

## Put comments here that give an overall description of what your
## functions do

# Input: a squared invertible matrix
# Output: stores the inverse of the matrix as long as it is not modified

## Returns a list of functions to set and get the inverse of the matrix x
makeCacheMatrix <- function (x=matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function () m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
        
}


## Uses the list of functions provided by makeCacheMatrix
## to compute or retrieve the inverse of matrix x

cacheSolve <- function(x,...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
