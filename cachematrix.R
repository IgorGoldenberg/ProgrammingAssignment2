## PLEASE NOTE:
## I use a different style of figure brackets where each open bracket appears on its own line
## I use long names for variables

## Write a short comment describing this function

## This function stroes the matrix passed (as "originalMatrix ), creates empty variable to 
## hold inverse Matrix ("inverseMatrix)
makeCacheMatrix <- function(originalMatrix = matrix()) {
	inverseMatrix<-NULL ##need to declare inverse of he matrix in the initialisation
	
	set<-function(newMatrix)
	{
		originalMatrix <<- newMatrix ## save new matrix
		inverseMatrix <<- NULL  ## After the matrix is passed, old inverse becomes invalid
					## it must be cleaned to avoid usage of outdated inverse
	}
	get<-function()
	{
		originalMatrix ## return the matrix
	}
	setInverse<-function(newInvMatrix)
	{
		inverseMatrix <<- newInvMatrix ## set inverse matrix (presumably calculated)
	}
	getInverse<-function()
	{
		inverseMatrix ## return inverse matrix
	}
	## return created functions in a list (with named values)
	list(	set = set,
		get = get,
		getInverse = getInverse,
		setInverse = setInverse)
}


## This function checks for the cached inverse. If found, it is returned with the warning message
## Otherwise the inverse matrix is calculated and saved
## Due to assignment constraints 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	currentInverseMatrix<-x$getInverse() ## get cached inverse
	if (!is.null(currentInverseMatrix)) ##check if inverse is cached already
	{
		message("useing cached matrix inverse") ## notify user that cached inverse is used
							## ideally it should be based on input function parameter
							## but I must follow assignment constraints
		
		return(currentInverseMatrix)  ## return statement is necessary to terminate the execution of the function
				## it also alleviates the need for "else" clause
	}
	currentInverseMatrix<-solve(x$get())    ## calculate the inverse of the stored data
	x$setInverse(currentInverseMatrix)      ## store calculted inverse. That the point of caching!
	currentInverseMatrix 			## return newly calculated and cached value
}
