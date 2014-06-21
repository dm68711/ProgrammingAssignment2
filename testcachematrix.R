source("cachematrix.R")

#
# testCacheMatrix - given a cached matrix, confirm we can compute and cache its inverse
#

testCacheMatrix <- function( cm ) {
  if(is.null(cm)) {
    print("cached matrix is NULL")
    return
  }
  
  #
  # confirm inverse is computed and cached on first iteration, and that the cached 
  # version is used for subsequent iterations
  # 

  for( i in 1:3 ) {
    print( "-----------------------------")
    print( sprintf( "iteration %d", i ) )

    mI <- cacheSolve(cm)
    print("originalMatrix: ")
    print(cm$get())
    print("inverseMatrix: ")
    print(mI)
    print("confirmation: ")
    print( mI %*% cm$get() )

    # assert the result of multiplying the matrix by its inverse is the identity matrix
    stopifnot( mI %*% cm$get() == diag( nrow(cm$get())) ) 
  }

  print( "============================" )

  #
  # confirm cached inverse is invalidated if a new matrix is assigned
  #

  originalCachedMatrix <- cm
  cm$set(diag(3))
  print("updated matrix:")
  print(cm$get())
  print("updated inverse:")
  print(cacheSolve(cm))
}


#
# test cases: invertible square matrices
# expected result: inverse computed and cached
#

testCacheMatrix( makeCacheMatrix( diag(3) ) );
testCacheMatrix( makeCacheMatrix( matrix( c(4,0,0,0,4,0,0,0,4), nrow=3, ncol=3 ) ) )

#
# test case: non-square matrix
# expected result: behavior was undefined in the problem ; opting to print a warning
# and return
#

testCacheMatrix( makeCacheMatrix( matrix( c(1,1,1,1,1,1), nrow=3, ncol=2 ) ) )

#
# test case: non-invertible matrix
# expected result: solve will raise an error, halting execution
#

testCacheMatrix( makeCacheMatrix( matrix( c(0,0,0,0,0,0,0,0,0), nrow=3, ncol=3 ) ) )

