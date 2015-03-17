# This code is designed to return the inverse of a matrix or, 
# when the inverse is already calculated, return the cached value.

# The makeCacheMatrix function is to convert the matrix into a list of functions
# that can set and get the matrix and the inverse of the matrix which are saved
# in the enveronment of the list.


makeCacheMatrix <- function(x = matrix()) {

    stored_inverse <- NULL
    
    # by calling the set function we can change the saved matrix in the list
    # and will also reset cached inverse matrix. The <<- makes sure the variables
    # go into the parent environment.
    #   For example when we have matrix y and use cx$set(y) we will put matrix y
    #   instead of matrix x in cx and directly reset the stored inverse matrix.
    
    
    set <- function(matrix){
        x <<- matrix
        stored_inverse <<- NULL
    }
    
    # stores the suplied matrix in the get variable
    
    get <- function()x
    
    # assigns a new value to the stored_inverse in the parent environment
    
    setinverse <- function (sent_replacement_inverse){
        stored_inverse <<- sent_replacement_inverse
    }
    #when called returns the stored inverse
    
    getinverse <-function(){
        return (stored_inverse)
    }
    
    # the list of functions.
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function takes the made list of functions and checks if the inverse is
# already present in the list environment. if not it calculates the
# inverse and "saves" it to the list environment 

cacheSolve <- function(madeMatrix, ...) {
    
    local_inverse <- madeMatrix$getinverse()
    
    #check if there is already an inverse present in the list environment 
    #if yes, get it
    if(!is.null(local_inverse)){
        message("getting cached data")
        
    }
    # if not present calculate the inverse and store it in the list environment
    else {
        local_data <- madeMatrix$get()
        local_inverse <- solve(local_data, ...)
        
        madeMatrix$setinverse(local_inverse)
    }
    #return the inverse of the matrix
    return(local_inverse)
    
}