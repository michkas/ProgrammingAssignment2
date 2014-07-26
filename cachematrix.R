## Functions 'makeCacheMatrix' and 'cacheSolve' provide a way for storing 
## and accessing a numerical (invertible) matrix along with its computed 
## inverse. This primarily serves as a way of avoiding costly recomputations of
## the inverse.
##
## More specifically, 'makeCacheMatrix' constructs and returns a "special matrix 
## object". The object state comprises of the internally stored matrix value and
## the value of its inverse matrix. The state of the object is accessible and
## modifiable by the set of functions returned by 'makeCacheMatrix'.
##
## 'cacheSolve is an adapted function based on 'solve' that can access objects
## created with 'makeCacheMatrix' and either return the already computed and
## cached value of the inverse stored in those objects (avoiding recomputation)
## or compute the inverse, store (cache) it in the object and return its value.




## 'makeCacheMatrix(mat)' creates an object suitable for storing a numeric 
## matrix and its inverse (assuming that the matrix is invertible). 
##
## It returns a list of the object functions 'set', 'get', 'setInverse' and
## 'getInverse' which are used to set, get the value of the matrix and to 
## set, get the value of its inverse matrix respectively.
##
## 'mat' is an  optional argument that initializes the value of the stored 
## matrix to the supplied value (mat input is always assumed to be an 
## invertible matrix). Unless a value is supplied for mat, mat defaults to the
## empty matrix (1X1 NA).


makeCacheMatrix <- function(mat=matrix(numeric(0),1,1))
{
        ## The inverse of mat is initialized to NULL
        matInv <- NULL
        
        ## set : Object function to set the value of the stored matrix
        set <- function(new_mat) {
                ## assign new_mat value to mat in the parent environment
                mat <<- new_mat
                ## and re-initialize the corresponding inverse matrix
                matInv <<- NULL
        }
        
        ## get : Object function to get the value of the stored matrix 
        get <- function() {
                mat
        }
        
        ## setInverse : Object function to set the value of the inverse matrix
        setInverse <- function (computed_inverse) {
                ## assign computed_inverse value to matInv in the parent
                ## ... environment
                matInv <<- computed_inverse
        }
        
        ## getInverse : Object function to get the inverse matrix
        getInverse <- function() {
                ## returns the inverse (or NULL if the inverse has not been 
                ## ... computed)
                matInv
        }
        
        ## Finally, return the list of set, get, setInverse and getInverse
        ## ... functions of the object
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)     
}


## 'cacheSolve(x)' returns the inverse of the matrix stored in the object 'x'. 
## 
## 'cacheSolve(x)' will compute the inverse if necessary, unless the latter has 
## already been computed and stored (cached) in 'x'. Upon computation, the 
## inverse matrix will also be stored in 'x' to save for future recomputations.
##
## Argument 'x' is required and is an object constructed by 'makeCacheMatrix'.

cacheSolve <- function(x, ...)
{        
        ## Obtain the value of the inverse stored in x via x$getInverse
        mInv <- x$getInverse()
        
        ## if mInv is not NULL, then the inverse is already computed and cached
        if(!is.null(mInv)) {
                message("getting cached data")
                ## return cached inverse
                return(mInv)
        }
        
        ## At this point, the function has not returned (i.e. mInv is NULL)
        ## ... and the inverse needs to be computed.
        ## Access the matrix stored in x via x$get and compute its inverse
        ## ... with solve
        x_mat <- x$get()
        mInv <- solve(x_mat, ...)
        ## Store the value of the inverse in x via x$setInverse
        x$setInverse(mInv)
        ## Print (return) computed inverse
        mInv
}