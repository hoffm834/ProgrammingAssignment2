
## This function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        j<-NULL
        set<-function(y){
                x<<-y
                j<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) j<<- solve
        getmatrix<-function() j
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix 
## If inverse has already been calculated, cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x=matrix(), ...) {
        j<-x$getmatrix()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        matrix<-x$get ()
        j<-solve(matrix, ...)
        x$setmatrix(j)
        j
} 
