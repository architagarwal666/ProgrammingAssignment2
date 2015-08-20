
#Actual Assignment
#Learn how to get inverse

mat1 <- matrix(c(2,3,5,1,7,8,2,5,1),nrow = 3, ncol =3)
mat2 <- matrix(c(2,3,1,1,7,8,2,5,1),nrow = 3, ncol =3)

inv_mat1 <- solve(mat1)

makematrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- solve(y)
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

z<-makematrix(mat1)


cachematrix <- function(x,newmatrix=matrix()) {
        data <- x$get()
        inv <- x$getinv()
        if(all(newmatrix == data) && !is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        inv <- solve(data)
        x$setinv(inv)
        inv
}

cachematrix(z,mat2)
