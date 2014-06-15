## Create a matrix, store cached values for matrix calculations, compute matrix calculations

## makeCacheMatrix provides get and set functions for matrix construction
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) m<<-inverse
    getInverse<-function() m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) 
}

## cacheSolve first attempts to pull stored values for matrix inversion calcs
## if there are no stored values, it computes matrix inversions
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}
