## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. These functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setInversion<-function(inversion) m<<-inversion
	getInversion<-function() m
	matrix(set=set, get=get, setInversion=setInversion, getInversion=getInversion)
}


## The function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix'. If the inverse has already been calculated (and the
## matrix has not changed), then 'cacheSolve' should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
	m<-x$getInverse()
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}
	matrixData<-x$get()
	m<-solve(matrixData)
	x$setInversion(m)
        ## Return a matrix that is the inverse of 'x'
	m
}
