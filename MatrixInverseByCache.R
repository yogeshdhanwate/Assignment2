#Function which makes cache matrix
makeCacheMatrix <- function(myMatrix = matrix()){
	# Initilize to NULL
	m <- NULL
	set <- function(y){
		myMatrix <<- y
		m <<- NULL
	}
	
	get <- function(){
		myMatrix
	}

	setInverse <- function(Inverse){
		m <<- Inverse
	}
	
	getInverse <- function(){
		m
	}

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
