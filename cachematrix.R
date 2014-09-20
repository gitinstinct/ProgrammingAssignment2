####---- GENERAL COMMENTS ----####

## makeCacheMatrix() and cacheSolve() combined take an invertible matrix and return its inverse in 1 of 2 ways:
## 1) if inverse is calculated in makeCacheMatrix(): by returning the matrix as cached by makeCacheMatrix
## 2) if inverse is not calculated in makeCacheMatrix(): by calculating the inverse in cacheSolve()

####---- FUNCTION COMMENTS ----####

## makeCacheMatrix() takes a matrix as argument, tracks whether that matrix has been inverted, and returns a list of FUNs applied to that matrix.
## Note: the matrix must be invertible

####---- Function 1/2: makeCacheMatrix()----####

makeCacheMatrix <- function(a_matrix = matrix()) {
    inverse_exists <- NULL                        		###inverse_exists is a flag; default value is NULL (i.e. no inverse calculated)
	setmatrix <- function(another_matrix){              ###pseudocode: object_name$setmatrix(new_matrix) lets you change the matrix used by the FUN
	a_matrix <<- another_matrix                         ###replaces previous matrix with new matrix 
	inverse_exists <<- NULL                             ###resets flag to NULL (previous matrix may have had inverse calculated; new matrix won't have)
	}

	getmatrix <- function() a_matrix                    ### object_name$getmatrix() allows you to see the current matrix being used by the function
	                                                    ###(good because the function itself returns a list) 
	setinverse <- function(value) inverse_exists <<- value ### changes the inverse_exists flag if an inverse has been calculated
	getinverse <- function() inverse_exists             ### gets the value of the inverse_exists flag
	list (setmatrix = setmatrix, getmatrix = getmatrix, ### the makeCacheMatrix() FUN returns a list of FUNs for cacheSolve() to access & modify
	setinverse = setinverse, getinverse = getinverse)
}

####---- FUNCTION COMMENTS ----####

## cacheSolve returns the inverse of a given invertible matrix:
## 1) when returning for the first time: by calculating & caching the inverse
## 2) when returning for the 2nd + time: by retrieving the stored inverse from the cache

####---- Function 2/2: cacheSolve()----####

cacheSolve <- function(a_matrix, ...) {
## Return a matrix that is the inverse of 'a_matrix'
        inverse_exists <- a_matrix$getinverse() ### tests whether an inverse of the given matrix has been calculated before
        if(!is.null(inverse_exists)) {			### i.e. if an inverse matrix has been calculated & cached already, then:
                message("getting cached data")  ### tell user that inverse matrix  is being retrieved from cache
                return(inverse_exists)          ### return inverse matrix from cache
        }
        else                            		### but if no inverse has been calculated:
		message("calculating inverse")          ### tell user that the operation is being performed for T1
        input_matrix <- a_matrix$getmatrix()       ### choose a matrix to invert
        inverse_exists <- solve(input_matrix, ...) ### feed this matrix into the invert function
        a_matrix$setinverse(inverse_exists)        ### change the state of inverse_exists (because an inverse has now been calculated); cache the inverse
        inverse_exists                          ### return the value of inverse_exists flag
}
###---- END OF FUNCTIONS AND COMMENTS ----####
