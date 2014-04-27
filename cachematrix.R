## This code exemplifies a programming approach to reduce the CPU processing
## by caching a global value for repeated calculations, and re-using such cached values
## The set/get of matrix values are modeled after the example in the assignment
## The set/get of i_matrix (inverse matrix) are modeled after the example in the assignment

makeCacheMatrix <- function(x = matrix()) {
	# begin by clearing the i_matrix
	i_matrix <- NULL
	
	# copy the user argument y into matrix
	set<-function(y){
		#set the global x value
		x<<-y
		#clear the global i_matrix
		i_matrix <<- NULL
	}
	
	# collect the vale of matrix
	get<-function() x
	
	# calculate i_matrix (inverse matrix) using the R function solve()
	set_i_matrix<- function(solve) i_matrix <<- solve
	
	# collect i_matrix value
	get_i_matrix<-function() i_matrix
	
	# create the function list
	list (set=set, get=get, set_i_matrix=set_i_matrix, get_i_matrix=get_i_matrix)
}


## In this example,  makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix() above
## If the inverse value has already been calculated (and the matrix has not changed), then 
## cacheSolve() retrieves the inverse from the cached value

cacheSolve <- function(x, ...) {
        
        # begin by collecting the i_matrix (inverse matrix)
        i_matrix <- x$get_i_matrix()
        
        # is the i_matrix already been cached? if so, then return cached value
        if ( !is.null (i_matrix)){
        	message ("getting cached data")
        	return (i_matrix)
        }
        
        # calculate i_matrix (inverse matrix)
        i_matrix<-solve(x$get(), ...)
        
        # cache the i_matrix
        x$set_i_matrix(i_matrix)
        
        # return i_matrix (inverse matrix of original passed value)
        i_matrix
}



# Use Assignment 2 sample code to create a function that makes a vector
makeVector <- function(x = numeric()) {
		# set the value of m (mean) to NULL
        m <- NULL
        # set the value to user inputed value of y
        set <- function(y) {
        		# use global variable x
                x <<- y
                #clear the value of m (mean)
                m <<- NULL
        }
        # collect the value of vector
        get <- function() x
        
        # calculate value of mean
        setmean <- function(mean) m <<- mean
        
        # collect the mean
        getmean <- function() m
        
        # create the function list
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
		# collect the mean
        m <- x$getmean()
        # if m (mean) value has been calculated and cached, then return cached value, otherwise calculate it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # get the data, and calculate mean, only if m (mean) was not cached
        data <- x$get()
        m <- mean(data, ...)
        
        # cache the m (mean) after calculating it
        x$setmean(m)
        
        # return the m (mean), after it has been cached
        m
}
