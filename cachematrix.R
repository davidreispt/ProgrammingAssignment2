## These functions allow create a special object that cache a matrix and returns its inverted matrix 

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #validates if the object is matrix
        if (!is.matrix(x)){
            stop("object is not a matrix")
        
        #validates if the matrix is squared and throws error if not
        } else if (nrow(x)!=ncol(x)) {
            stop("not a squared matrix")
            
        #nullifies m 
        } else m <- NULL
        
        #set function - store matrix 
        set <- function(y) {
                m <- NULL
                x <<- y
        }
        
        # returns original matrix       
        get <- function() x
        
        # stores the inverted matrix
        setimatrix <- function(imatrix) m <<- imatrix
        
        # returns the inverted matrix
        getimatrix <- function() m
        
        # returns list of function to operate cacheMatrix object
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix and returns the cached value if it exists   
cacheSolve <- function(x, ...) {
        # get the matrix from the funcion on object x
        m <- x$getimatrix()
        
        # checks if the objects exists (was cached already)
        if(!is.null(m)) {
                ## Return the cached matrix that is the inverse of 'x'
                return(m)
        }
        
        # get the matrix from the funcion on object x
        data <- x$get()
        
        # solves the matrix 
        m <- solve(data, ...)
        
        #sets(caches) the inverse matrix on object x using the set function
        x$setimatrix(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
