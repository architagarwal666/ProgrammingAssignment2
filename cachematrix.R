#First Part
#function to create a special "matrix" object
#that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #initialize the inverse as NULL
        set <- function(y) {   #function to cache the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x    #function to retrieve the value of the matrix
        setinv <- function(inverse) inv <<- inverse    #function to cache the inverse of matrix
        getinv <- function() inv    #function to retrieve the value of the inverse of matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



z<-makeCacheMatrix(mat1)

#Second Part
#function to computes the inverse of the special "matrix" 
#returned by `makeCacheMatrix`

cacheSolve <- function(x, z) {
        data <- z$get() #Get the old matrix
        inv <- z$getinv()  #Get inverse of the old matrix
        if(all(x == data) && !is.null(inv)) {  #Check if matrix has changed and inverse has been calculated or not
                message("getting cached data") #If the inverse was calculated then retrieve it
                return(inv) #return the cache inverse, since the old and new matrix are same
        }
        
        if(is.null(inv)){ #check if inverse of the old matrix has been calculated or not
                inv <- solve(data) #If the inverse has not been calculated, then calculate the inverse
                z$setinv(inv) #set the inverse
                inv
        } else {  #If the inverse of old has been calculated, then calculate the inverse of new
                inv2 <- solve(x) #return the inverse of new
                return(inv2)
        }
}

cacheSolve(mat1,z)
