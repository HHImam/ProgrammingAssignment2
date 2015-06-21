## R Programming - Coursera
## Programming Assignment 2
## Write a pair of functions that cache the inverse of a matrix. The functions are:
## 1- makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 2- cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## The makeCacheMatrix function creates a list containing a function that computes the following:
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  	m<-NULL
  	set<-function(y){
  	x<<-y
  	m<<-NULL
	}
	get<-function() x
	setmat<-function(solve) m<<- solve
	getmat<-function() m
	list(set=set, get=get,setmat=setmat,getmat=getmat)

}

## The cacheSolve function returns the inverse of the matrix.
## The matrix provided must be square and invertible.
## First check if the inverse has already been computed. 
## If it is computed, get that result via gemat(). 
## If not, compute the inverse and set the value in cache via setmat().

cacheSolve <- function(x, ...) {
	m<-x$getmat()
      if(!is.null(m)){
      	message("getting cached data")
     		return(m)
    	}
 
    	matrix<-x$get()
    	m<-solve(matrix, ...) ## solve(x) computes the inverse of a square, invertible matrix.
   	x$setmat(m)
   	m

}
