#######################################################################
## Two functions that caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m=matrix()){
        ## create blank inverse object
        inv <- NULL
        ## set the matrix
        set <- function(matrix){
                m <<- matrix
                inv <<- NULL
        }
        ## get the matrix
        get <- function(){
                ## return the matrix
                m
        }
        ## set the inverse of the matrix
        setInverse <- function(inverse){
                inv <<- inverse
        }
        ## get the inverse of the matrix
        getInverse <- function() {
                ## return the inverse object
                inv
        }
        ## return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieve the inverse from
## the cache.
cacheSolve <- function(x,...){
        ## a matrix that is the inverse of x
        m <- x$getInverse()
        ## return the inverse if has been already set
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## get the matrix from our object
        data <- x$get()
        ## compute the inverse
        m <- solve(data)
        ## set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}

#######################################################################
## TEST
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
