## First, you need to set a sample matrix, such matrix has to be a 
## SQUARE MATRIX

##The makeCacheMatrix is a function caching the sample matrix
##And the cacheSolve funtion is use to solve your sample 
##by searching cache first then solve it.


##The makeCacheMatrix function sets a indicator "m" that indicate 
##statue of inverse Sample matrix is available or not. 
##Other information will be store at list x.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve)m <<- solve
        getsolve <- function() m
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve
        )
}

##The cacheSolve funtion need a makeCacheMatrix list as the argument. 
##It will search the indicator(which is "m" in makeCacheMatrix), 
##if the m has a value, it will present "m", 
##if not, the function will calculate the solve of 
##input matrix in makeCacheMatrix and store it in "m" as cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


#Run Code Below to test fuction

Sample<-matrix(rnorm(16),4,4)

Test<-makeCacheMatrix(Sample)

cacheSolve(Test)

cacheSolve(Test)