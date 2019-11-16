## These two functions calculate and cache the inverse of matrixes.

makeCacheMatrix <- function(x = matrix()) {
        invm<- NULL         ## give the initial value to inverse matrix
        set<- function(y){  ## set the value of matrix
                x<<- y
                invm<<- NULL
        }
        get<- function() {  ## get the value of matrix
                x
        }
        
        setinv<- function(solve) {  ## set the inverse matrix
                invm <<- solve
        }
        
        getinv<- function() {   ## get the inverse matrix
                invm
        }
        
        list(set= set, get= get, setinv= setinv, getinv= getinv)
}


## If the inverse of a matrix is calculated before and the matrix
## remains the same, this function will return the cached inverse
## matrix. Otherwise, it will calculate the inverse matrix and cache 
## it.

cacheSolve <- function(x, ...) {
        invm<- x$getinv()
        if(!is.null(invm)){  ## check if matrix is calculated before
                message("getting cached data")
                return(invm)
        }
        data<- x$get() 
        invm<- solve(data, ...) ## calculating inverse matrix
        x$setinv(invm) ## set the inverse matrix
        invm 
        ## Return a matrix that is the inverse of 'x'
}
