## Put comments here that give an overall description of what your
## functions do
        
        # makeCacheMatrix initializes a matrix, clears the cache of any 
        # previous calculations, establishes the getters and setters
        # of our code, and returns a list with the matrix and the empty
        # cache in its environment
        
        # cacheSolve checks to see if the cache is not empty, then returns the
        # cache value if it is not empty, or calculates the inverse of the 
        # given matrix if it is empty

        # running the code below will give you an example of how cacheSolve
        # calculates the inverse on its first call, and uses the cache 
        # on its second call
                # > my_matrix <- matrix(c(2, 7, 2, 8), 2, 2)
                # > my_matrix_object <- makeCacheMatrix(my_matrix)
                # > cacheSolve(my_matrix_object)
                # > cacheSolve(my_matrix_object)

## Write a short comment describing this function
        # makeCacheMatrix is identical to makeVector except for the fact that
        # it initializes a matrix, and its listed functions are renamed to
        # make the new objective (i.e. inversion of a matrix) explicit

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
        # cacheSolve is identical to cachemean except for the fact that
        # it calls a function to invert a matrix instead of calculating a mean

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
