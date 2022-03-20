## This function creates an enclosure (matinv) which will capture and store the inverse of a matrix inside this function (as opposed to inside of the global environment). In a nutshell, we are 
## creating a function within a function.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL                                     ## starting with matinv being empty; it will be replaced by the later operations 
        set <- function(y) {
                x <<- y                                    ##the double arrow operator keeps looking up the chain of parent environments until it finds the name that matches
                matinv <<- NULL
        }
        get <- function() x                                ## stores x in the enclosing environment
        setInverse <- function(inverse) matinv <<- inverse ## this will calculate the inverse of the matrix
        getInverse <- function() matinv                    ## stores 'matinv" in the 'getInverse" environment
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)                      ##makes the list of all four functions where "x" and "matinv" are stored
        
}


## Function below will calculate the inverse of the matrix unless it has been already calculated

cacheSolve <- function(x, ...) {
        matinv <- x$getInverse()
        
        if (!is.null(matinv)) {                     #this step checks whether inverse has been calculated already
                message("getting cache")            # if it has (is not null), then the inverse will be returned. 
                return(matinv)
        }
        mat <- x$get()
        matinv <- solve(mat, ...)
        x$setInverse(matinv)
        matinv
        
}

##Checking my code:

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

###Yahooo! It works!!!

