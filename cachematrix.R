## Functions below cahce the inverse of a special "matrix"

## The First function listed below will:
## - set the value of the matrix
## - get the value of the set matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

## The second function listed below will:
## - compute the inverse if the special "matrix" created in function 1
## - If the inverse of functions 1 matrix hasn't changed an dis available it will return the
## inverse from teh cache


## This function will create a special "matrix" than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        matrixinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
                set = set,
                get = get,
                matrixinverse = matrixinverse,
                getinverse = getinverse
        )
}


## Calculate the inverse of the special "matrix" created with function 1

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting chached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$matrixinverse(i)
        i
}