## This function takes argument Matrix.  
## This function doesnt check whether matrix is invertable or not. 
## Function has getters & setters for both inverted matrix & original matrix.
## As fuction should return some thing.. In this case it returns a list which 
## holds all functions as list. 

makeCacheMatrix <- function(x = matrix()) {
    
    invertedMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    
    get <- function() x
    
    getInvertMatrix <- function() invertedMatrix
    
    setInvertMatrix <- function(im) invertedMatrix <<- im
    
    list(set = set, get = get,
         setInvertMatrix = setInvertMatrix,
         getInvertMatrix = getInvertMatrix)
}


## This function takes argument as makeCacheMatrix. 
## In case if Inverted matrix is already avaliable 
## it will return the same else it will calculate 
## and sets invertedMatrix within object. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sm <- x$getInvertMatrix()
    if(!is.null(sm)) {
        message("getting cached data")
        return(sm)
    }
    
    originalMatrix <- x$get()
    
    sm <- solve(originalMatrix, ...)
    
    x$setInvertMatrix(sm)
    
    sm
}


# ## Testing Code
# ## Below line creates Matrix of 2 * 2
# inputMatrix <- matrix(1:4, 2, 2)
# ## Creating CacheMatrix Object
# cacheMatrix <- makeCacheMatrix(inputMatrix)
# ## Invoking cacheSolve to calculate Inverse Matrix
# cacheSolve(cacheMatrix)
# ## Testing the for Identity Matrix & message stating Cached Data
# cacheSolve(cacheMatrix) %*% cacheMatrix$get()