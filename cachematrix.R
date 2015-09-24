## This function creates the inverse of a matrix using the solve function
## m<-Null, creates an empty variable for later use
## set <- function(y).... creates a function that sets matrix x to y and sets matrix m to null. y and m are now in memory in another environment, i.e. cache
## get <- function() x creates a fuction returning matrix x
## setmatrix <- function (solve) m<<-solve creates a function to solve, i.e. inverts the matrix
## getmatrix <- function() creates a function to return the inverse matrix m
## list(set = set.......) creates a vector returning the names of the functions
##
## to use this function:
## myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
##  myMatrix$getmatrix() shows the matrix just created

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get=get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)        
        
}


## This function is used to call the object created by the makeCachematrix
## if the matrix m is already populated then display the message "getting cached data" and get m from the cache
## otherwise get the matrix, run the inverse and set the matrix m in cache for future use

## use this function by calling:
## c <- cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmatrix(m)
}