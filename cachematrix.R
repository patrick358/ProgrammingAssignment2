# This code is designed to 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    stored_inverse <- NULL
    
    set <- function(y){
        matrix <<- y
        stored_inverse <<- NULL
    }
    
    get <- function(){
        return (matrix)
    }
    setinverse <- function (sent_replacement_inverse)
        stored_inverse <<- sent_replacement_inverse
    
    getinverse <-function(){
        return (stored_inverse)
    }
    
    list(set=set, get=get,
         setinverse = setinverse
         getinverse = getinverse)
}

# Write a short comment describing this function

cacheSolve <- function(madeMatrix, ...) {
    
    local_inverse <- madeMatrix$getinverse()
    
    if(!is.null(local_inverse)){
        message("getting cached data")
    }
    else {
        local_data <- madeMatrix$get()
        local_inverse <- solve(local_data, ...)
        
        madeMatrix$setmean(local_inverse)
        return(local_inverse)
    }
}