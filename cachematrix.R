## Fuction description
## This a a pair of fucntion to cache the inverse of a matrix. The when asked
## to do inverse of a matrix, the functions will check to see if the inverse
## is already stored in cache or not. If the inverse is already existed in the 
## cache, the function will try to retrieve it. Otherwise, the function will
## calculate the inverse

## makeCacheMatrix is a function to return the list of fucntion to
# - Set the value of matrix
# - Get the value of matrix
# - Set the value of the matrix inverse
# - Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        #Initial cache, Null is default
        inv <- NULL
        #Matrix Setter
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        #Matrix Getter
        get <- function() x
        #Inverse Setter
        setinv <- function(solve) inv <<- solve
        #Inverse Getter
        getinv <- function() inv
        #Construct table for inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve check to see if inverse existed in cache
## If inverse existed, return that value
## If not, calculate the inverse using solve() function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ##check to see if inverse existed
        if(!is.null(inv)){
                message("Inverse existed")
                return(inv)
        }
        ##Calculate the inverse
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        
        #Return inverse of matrix
        inv
}
