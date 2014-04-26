## The first function makeCacheMatrix creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x=matrix())
{
inv <-NULL
set<- function(y)       ## Set the value of the matrix
	{
		x<<- y
		inv<<-NULL
	}
get<-function()x        ## Get value of  the matrix
set_inv<-function(inversion) inv <<- inversion  ## Set value of the inversed matrix
get_inv <- function()inv                ## Get value of the inversed matrix
list(set=set,get=get,                   ## Set the list of all the functions
	set_inv=set_inv,
	get_inv=get_inv)

}



## CcacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. 
## If theinverse has already been calculated (and the matrix has not been changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <-function(x)
{
	inv<-x$get_inv()
	if(is!null(inv)) ## Verify if there's an already cached matrix
	{
		message("getting previously cached data")
		return(inv)
	}
	data<-x$get
	inv<-solve(data) ## inverse the matrix
	x$set_inv(inv) ## cache inversed matrix
	inv

}
