source("cachematrix.R")

#
# testCacheMatrix - given a cached matrix, confirm we can compute and cache
# its inverse
#

testCacheMatrix <- function( cm ) {
  if(!is.list(cm)) {
    warning("testCacheMatrix: argument is not a list")
    return (NA)
  }

  #
  # confirm inverse is computed and cached on first iteration, and that 
  # the cached version is used for subsequent iterations
  # 

  for( i in 1:3 ) {
    print( "-----------------------------")
    print( sprintf( "iteration %d", i ) )

    # 
    # no cached value prior to first cacheSolve call
    # cached value after first cacheSovle call
    #

    if( i == 1 ) {
       stopifnot( cm$get() == NULL )
    }
    else {
       stopifnot( cm$get() != NULL ) 
    }

    #
    # compute and confirm the inverse
    #

    mI <- cacheSolve(cm)
    print("originalMatrix: ")
    print(cm$get())
    print("inverseMatrix: ")
    print(mI)
    print("confirmation (identity matrix): ")
    print( mI %*% cm$get() )

    #
    # assert the result of multiplying the matrix by its inverse is 
    # the identity matrix; use round to account for floating point 
    # precision errors
    #

    r = nrow( cm$get() )
    c = ncol( cm$get() )
    stopifnot( r == c )
    stopifnot( matrix(round(mI %*% cm$get(), 8), r, c) == diag( nrow(cm$get())) ) 
  }

  print( "============================" )

  #
  # confirm cached inverse is invalidated if a new matrix is assigned
  #

  originalCachedMatrix <- cm
  cm$set(diag(3))
  stopifnot( cm$getInverse() == NULL )
  print("updated matrix:")
  print(cm$get())
  print("updated inverse:")
  print(cacheSolve(cm))
}


#
# test case: initial cache returns NULL until inverse is computed
#

cm <- makeCacheMatrix( diag(3) )
stopifnot( cm$get() == NULL )
print("initial cache returns NULL")

mI <- cacheSolve(cm)
stopifnot( cm$get() != NULL );
stopifnot( mI != NULL );
print("cached matrix returns non-NULL after solution is computed")

#
# test cases: invertible square matrices
# expected result: inverse computed and cached
#

testCacheMatrix( makeCacheMatrix( diag(3) ) );
testCacheMatrix( makeCacheMatrix( matrix( c(4,0,0,0,4,0,0,0,4), 3, 3 ) ) )
testCacheMatrix( makeCacheMatrix( matrix( c(4,3,3,2), nrow=2, ncol=2 ) ) )
testCacheMatrix( makeCacheMatrix( matrix( c(1,2,3,0,4,5,1,0,6), 3, 3 ) ) )

#
# test case: non-square matrix
# expected result: behavior was undefined in the problem ; opting to 
# issue a warning and return NA
#

print("testing with non-square matrix");
testCacheMatrix( makeCacheMatrix( matrix( c(1,1,1,1,1,1), 3, 2 ) ) )

#
# test case: non-invertible matrix
# expected result: behavior was undefined in the problem ; opting to 
# issue a warning and return NA
#

print("testing with non-invertible matrix");
testCacheMatrix( makeCacheMatrix( matrix( c(0,0,0,0,0,0,0,0,0), 3, 3 ) ) )

