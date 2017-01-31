## the functions below can build a matrix whose inverse
## is cached

## 
## makeCacheMatrix creates an object that stores a matrix and its inverse. It creates a list of functions that set and get the matrix value and set get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
	set<-function(y){
		x<<-y
		i<--NULL
	}
	get<-function() x
	setinverse<-function(inverse) i<<-inverse
	getinverse<-function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of a matrix (the matrix is supposed invertible). Firstly, it checks if the inverse matrix has been already evaluated if this is the case
##it retrieves the value and returns the result if this is not the case then it calculates the inverse and prints the result

cacheSolve <- function(x, ...) {
         i <- x$getinverse()

    if(!is.null(i)) {

        message("getting cached data.")

        return(i)

    }

    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i

     }
