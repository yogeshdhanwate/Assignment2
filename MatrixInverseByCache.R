#Function which makes cache matrix
makeCacheMatrix <- function(myMatrix = matrix()){
	# Initilize to NULL
	m <- NULL
	# Function which set the Matrix & make matrix inverse to NULL.
	set <- function(y){
		myMatrix <<- y
		m <<- NULL
	}
	
	# Function which return the Matrix.
	get <- function(){
		myMatrix
	}

	# Function which set Matrix inverse.
	setInverse <- function(Inverse){
		m <<- Inverse
	}
	
	# Function which return Inverse of Matrix.
	getInverse <- function(){
		m
	}

	# Returning list of functions for setting, getting Matrix & Matrix Inverse.
	list(set = set, get = get, setInverse = setInverse,
			getInverse = getInverse)
}

# Function which compute the inverse of matrix & cache it.
cacheSolve <- function(myMatrix){
	# Getting the cached inverse of matrix
	m <- myMatrix$getInverse()
	# if cached matrix inverse is not NULL i.e. inverse of matrix is computed
	# already return the cached matrix inverse.
	if(!is.null(m)){
		message("Getting cached matrix inverse")
                return(m)
        }
	# Get the matrix.
	data <- myMatrix$get()
	# compute the inverse
	m <- solve(data)
	# Put the matrix inverse on to cache.
	myMatrix$setInverse(m)
	m # return matrix inverse.
}
