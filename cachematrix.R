##Hello Madame or Sir. This is my solution to the second home assingment

## The following function "makeCacheMatrix" creates a list of functions, that
## allows to extract the inverse of the applied matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { ## set a new matrix
                x <<- y
                inv <<-NULL 
        }
        get <- function() x ## assing the matrix
        setinverse <- function(inverse) inv <<- inverse ## stores the inverse
        getinverse <- function() inv ## assing the inverse stored via setinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" solves the "Matrix" or list of programs produced by "makeCacheMatrix"
## It checks wether or not the inverse already has been calculated. If it is available in the
## cache, it returns the cached inverse. If it is not, the inverse is calculated and saved 
## by the setinverse function of the list created by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        inv <-x$getinverse() 
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv) ## if an inverse was already stored, it would be returned here
        }
        data <- x$get() ## if there was no inverse in the cache, the matrix is assigned here
        inv <-solve(data, ...) ## and its inverse is calculated and assigned here
        x$setinverse(inv) ## here the inverse is stored in the cache, so it must not be calculated again
        inv
}



t<- cbind(c(2,1), c(5,3))
t2<-makeCacheMatrix(t)
t2
t3<-cacheSolve(t2)
t3 
t4<-cacheSolve(t2)
#getting cached inverse. Wohooo it works!!!
t4 

#setting a new Matrix also works
t.new<- cbind(c(3,-1), c(-5,2))
t2$set(t.new)
t3.new<-cacheSolve(t2)
t3.new 



## All the best from Berlin
## Poligon89
