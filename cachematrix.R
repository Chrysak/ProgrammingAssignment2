## *********************************************************************************    
## ******* SHORT COMMENT ON WHAT makeCacheMatrix AND cacheSolve FUNCTIONS DO *******
## *********************************************************************************
##
##  The makeCacheMatrix(x) function defines, when it's called, a "special" matrix   
##  and its environment where a square matrix 'x' and its computed inverse will be 
##  stored when the cacheSolve(x,...) function will be called and will so compute the 
##  inverse once at least. 
## 
##  The cacheSolve(x,...) function does:
##     1) If the inverse of a square matrix 'x' was already computed and previously 
##       stored in the makeCacheMatrix(x)' environment (by a previous call of 
##       cacheSolve), it returns it 
##     2) Otherwise/Else (the inverse is NULL yet), it computes the inverse of the 
##        square matrix 'x' and stores it in the makeCacheMatrix(x)'s environment  
##
##                           !!!  GENERAL MESSAGE !!!
## !! For a better understanding, names of the m variable and of the functions given
## !! in the initial makeVector() and cachemean() examples were changed in my version:
## !!   - The m variable of makeVector() is renamed m1 in makeCacheMatrix(x)
## !!   - The m variable of cachemean() is renamed m2 in cacheSolve(A,...) 
## !!   - the set(), get(), setmean() and getmean() functions of the makeVector() 
## !!     example were redefined and renamed:
## !!         * setX(), getX(), setM1() and getM1() for the makeChaceMatrix(x)'s 
## !!           environment
## !!         * setA(), getA(), setInverseA() and getInverseA() for the working
## !!           environment  
##
## **********************************************************************************

##                      // FUNCTION: makeCacheMatrix(x) //         
##  When when the function is called:
##       1) The value of the matrix 'x' given as argument (previously set in 
##          a the calling environment) is stored in the function's environment
##       2) The matrix m1 is defined and set to NULL in the function's environment
##          It is not overwritten with the superassignment <<- until cacheSolve(x,...) 
##          is called for computing the inverse of 'x' and then storing it back in
##          the makeCacheMatrix()'s environment.  
##       3) The setX(), getX(), setM1() and getM1() are defined and returned to the 
##          other calling environment wih the new names setA(), getA(), setInverseA()
##          and getInverseA(). 
##          Only getX() and getM1() are executed when makeCacheMatrix(x) is called 
##          (y et m2 are free variables). 

makeCacheMatrix <- function(x = matrix()) {

  m1 <- NULL
  setX <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  getX <- function() x
  setM1 <- function(m2) m1 <<- m2
  getM1 <- function() m1
  list(setA = setX, getA = getX,
       setInverseA = setM1,
       getInverseA = getM1)  
  
}

##                         // FUNCTION: cacheSolve(A,...) //         
##  When when the function is called:
##       1) It retrieves the value of the m1 variable previously stored in the 
##          makeCacheMatrix's environment (m2 <- x$getInverseA()) 
##       2) If the value of the m1 is NULL, it then retrieves the square matrix 'x'
##          from the makeCacheMatrix's environment where it was also stored (data <- 
##          x$getA()) and computes its inverse before calling the setM1 function of 
##          makeCacheMatrix (with x$setInverseA(m2)) where and when the m1 variable 
##          is set to the new value of m2 thanks to the superassignment <<-
##       3) It returns the value of the inverse (m1 equals to m2 now) stored in m2 to
##          the working environment

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  
  m2 <- x$getInverseA()
  if(!is.null(m2)) {
    message("getting cached data")
    return(m2)
  }
  data <- x$getA()
  m2 <- solve(data, ...)
  x$setInverseA(m2)
  m2
  
}
