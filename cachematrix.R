
###############################################################
##creates a special "matrix" object that can cache its inverse.
###############################################################

makeCacheMatrix <- function(x = matrix()) {
            m <- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))            #create matrix of correct dimensions filled with NAs
            set <- function(y) {                                      #declare set subfunction
                    x <<- y
                    m <<- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))       #initialize m            
            }
            get <- function() x                                       #declare get subfunction
            setInverse <- function(solve) m <<- solve(x)              #declare setInverse subfunction
            getInverse <- function() m                                #declare getInverse subfunction                                
            list(set = set, get = get,                                #return object
                 setInverse = setInverse,
                 getInverse = getInverse)
    }

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            m <- x$getInverse()
            if(!all(apply(m,1,is.na))) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setInverse(m)
            m
    }


#####################################
#test data
############################################
# list <- c(1,2,3,0,1,4,5,6,0)
# mat <- matrix(list,nrow=3,ncol=3,byrow=T)

###########################################
# test run
##########################################
# cachedMatrix <- makeCacheMatrix(mat)
# cachedMatrix$get ()                   #should return the original matrix
# cachedMatrix$getInverse ()            #should return matrix of nas
# myInv <- cacheSolve(cachedMatrix)     #should return the inverted matrix
# cachedMatrix$getInverse ()            #should now return the inverted matrix

##########################################
# calculate correct answer directly for comparison
############################################
# inv <- solve(mat)