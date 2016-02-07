

# returns a list of functions, set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) myInverse <<- inverse
    getInverse <- function() myInverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#If the inverse has already been cached, it is retrieved, otherwise it is calculated.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    myInverse <- x$getInverse()
    if(!is.null(myInverse)) {
        message("getting cached data")
        return(myInverse)
    }
    data <- x$get()
    myInverse <- solve(data, ...)
    x$setInverse(myInverse)
    myInverse
}

# how to use the above

#define a square matrix
mat1 <- matrix(c(1,2,3,2,3,4,5,0,5), nrow = 3, ncol = 3)
# is it invertible?
solve(mat1)
# [,1] [,2] [,3]
# [1,] -1.5 -1.0  1.5
# [2,]  1.0  1.0 -1.0
# [3,]  0.1 -0.2  0.1
# yes

# now make a 'makeCacheMatrix' special matrix
specMat <- makeCacheMatrix()
print(specMat)
# $set
# function (y) 
# {
#     x <<- y
#     myInverse <<- NULL
# }
# <environment: 0x5403880>
#     
#     $get
# function () 
#     x
# <environment: 0x5403880>
#     
#     $setInverse
# function (inverse) 
#     myInverse <<- inverse
# <environment: 0x5403880>
#     
#     $getInverse
# function () 
#     myInverse
# <environment: 0x5403880>
   
#check what's in specMat
print(specMat$get())
# [,1]
# [1,]   NA
# It's not set yet

#set it now
specMat$set(mat1)
print(specMat$get())
# [,1] [,2] [,3]
# [1,]    1    2    5
# [2,]    2    3    0
# [3,]    3    4    5

print(cacheSolve(specMat))
# [,1] [,2] [,3]
# [1,] -1.5 -1.0  1.5
# [2,]  1.0  1.0 -1.0
# [3,]  0.1 -0.2  0.1

# and again...
print(cacheSolve(specMat))
# getting cached data
# [,1] [,2] [,3]
# [1,] -1.5 -1.0  1.5
# [2,]  1.0  1.0 -1.0
# [3,]  0.1 -0.2  0.1

