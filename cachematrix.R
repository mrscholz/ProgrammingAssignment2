## makeCacheMatrix and cacheSolve define methods that allow the storage of matrices and their inverse
## to the cache to avoid recalculation of the inverse and, hence, saves computational time 

## makeCacheMatrix(x) creates a list of methods to set/get the matrix x and its inverse to/from cache  

makeCacheMatrix <- function( x = matrix() ){ 
    invX <- matrix(data=rep(NA,nrow(x)*ncol(x)),nrow(x),ncol(x)) #define empty matrix of same dimensions as x
    
    #function to explicitly set/cache x and overwrite a previously cached inverse of x
    set <- function(y) {
        x <<- y
        invX <<- matrix(data=rep(NA,nrow(x)*ncol(x)),nrow(x),ncol(x))
    }
    
    get <- function() {
        x #call x
    }
    
    setInv <- function(solve) {
        invX <<- solve #calculate and cache invX
    }
    
    getInv <- function() {
        invX #call invX (from cache)
    }
    
    #return a list object with methods to get and set matrix and its inverse
    list(set=set,get=get,setInv=setInv,getInv=getInv) 
}


## cacheSolve(x) checks whether the inverse of x exists and returns the cached 
## inverse if yes, i.e., calculates the inverse if not. 

cacheSolve <- function(x,...){
    invX <- x$getInv()
    
    #check whether invX was calculated previously
    if(sum(is.na(invX)==0)){
        message("getting cached data")
        return(invX) #call from cache and stop execution 
    }
    
    #calculate inverse
    data <- x$get() #get original matrix data from list
    invX <- solve(data,...) #compute inverse of x
    x$setInv(invX) #store matrix in cache
    invX #return inverse
}
