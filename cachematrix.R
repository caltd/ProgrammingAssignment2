



#### makeCacheMatrix function has four functions; setMatrix, getMatrix, setInverse and getInverse.
#### setMatrix sets the mat variable to the argument passed and sets the cached value of inverse (inv) to NULL
#### getMatrix returns the stored matrix (mat)
#### getInverse returns trhe stored matrix inverse (inv)




makeCacheMatrix <- function(x = matrix()) {
	inv <<- NULL
	setMatrix <- function (m) {
		mat <<- m
		inv <<- NULL
	}

	getMatrix <- function() { mat }

	getInverse <- function () { inv }

	setInverse <- function (inverse) { inv <<- inverse }

	
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
	
	}





#### This function takes the return value of the makeCacheMatrix, checks whether the stored inverse (inv) is NULL.
#### If so the inverse is calculated and cached. If the stored (cached) inverse matrix (inv) is not NULL, then it is returned along with a message.
#### It also takes an optional argument x in which case the else block is executed.


cacheSolve <- function(fun, x = NULL,  ...) {
        ## Return a matrix that is the inverse
		tmp <- matrix()
		matrix <- fun$getMatrix()
		if (is.null(x)) {

			if (is.null(fun$getInverse())) {
					if (!is.null(matrix)) {
						tmp <- solve(matrix)				
						fun$setInverse(tmp)
						return(tmp)
					}
					else {
					print ("Matrix is NULL")
					return;
					}

			}
			
			message("The cached ans: ")
			fun$getInverse()
		
		}

		else {
				if (identical(matrix, x)) {
				message("The cached ans: ")
				fun$getInverse()

				}


				else {
					fun$setMatrix(x)
					tmp <- solve(x)
					fun$setInverse(tmp)
					tmp
				}
		}

			
		
}
