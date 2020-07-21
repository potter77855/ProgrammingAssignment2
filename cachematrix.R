##  CACHEMARIX HAS 2 FUNCTIONS:
##  1)  makeCacheMatrix - CREATES LISTING WITH FUNCTIONS TO:  
##     A) sets value of special matrix
##     B) gets value of special matrix
##     C) sets value of the inverse matrix
##     D) gets value of the inverse matrix
##  2)  cacheSolve - RETRUN INVERSE MATRIX OF INPUT MATRIX
makeCacheMatrix <- function(newinpmat=matrix())
{
##   KEEPING INPUT MATRIX BEFORE IT IS SET TO OUR INPMAT
    ne<-new.env()
    ne$inpmat <- newinpmat
##  WE WANT TO BE ABLE TO RETURN IT    
    getnewinpmat<-function() newinpmat <<- ne$inpmat
##  SET FUNCTION ASSIGNS SPECMAT AS 'DUMMY' ENTRY
    set <- function(specmat)
    {
##  inpmat = INPUT MATRIX ENTERED      
    inpmat <<- specmat
    ##  SET INVERSE MATRIX (invmat) TO NULL
    invmat <<-NULL
    }
##  RETURNS VALUE OF INVERSE MATRIX (invmat) VIA SOLVE FUNCTION
    get <-function() inpmat
    setinverse <- function(solved) invmat <<- solved
    getinverse <- function() invmat
    list(getnewinpmat=getnewinpmat, set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##  TEST OF 2 MATRICIES (mtx1 & mtx2) BEING EQUAL
##  TEST USED IN cacheSolve FUNCTION TO SAVE TIME  
test_identity <- function(mtx1,mtx2)
{
##  SET TEST FLAG TO TRUE  
    flag <- TRUE
##  IF DIFFERENT mtx1 -> SET FLAG TO FALSE      
    if    (!is.matrix(mtx1))
          {
          flag <- FALSE
          }
    else
##  IF DIFFERENT mtx2 -> SET FLAG TO FLASE       
          if    (!is.matrix(mtx2))
                {
                flag <- FALSE
                }
     if   (flag == TRUE)
          {
##   CHECK # ROWS & # COLUMNS OF mtx1 & mtx2      
           if   ((nrow(mtx1) == nrow(mtx2))
                &
                (ncol(mtx1) == ncol(mtx2)))
                {
##    COMPARE MATRIX (compmat) SET            
                compmat <- (mtx1 == mtx2)
##    LISTING OF '1' & '0' FOR REVIEW                
                complst <- as.list(compmat)
                if   (sum(as.integer(complst)) == length (complst))
                     {
                     flag <- TRUE
                     }
                else
                     {
                     flag <- FALSE
                     }
                }
           else
                {
                flag <- FALSE  
                }
     }
     return (flag)   
}          
## FUNCTION cacheSolve: 
##      RETRUNS VALUE OF INVERSE MATRIX 
##      WHICH WAS THE ARGUMENT OF FUNCTION makeCashMatrix
cacheSolve <- function(fnc,...)
{
    tryCatch(fnc$get(),
             error = function(e){message("an error fnc$get()\n",e)
                 fnc$set(fnc$getnewinpmat())
             },
             warning = function(w){message("a warning fnc$get()\n",w)},
             finally = {}
             
    )
    if(!is.null(fnc$get()))
    {message("nulls and set\n")}
## RETURN MATRIX THAT IS INVERSE OF inpmat  
     if   (test_identity(fnc$getnewinpmat(),fnc$get()))
          {
          invmat <- fnc$getinverse()
          }
     else
          {
          newm<-fnc$getnewinpmat()
          fnc$set(newm)  
          invmat<- fnc$getinverse()
          }
     if   (!is.null(invmat))
          {
          message ("getting cache data")
          return(invmat)
          }
     else
          {
##  holdmat JUST A HOILDING MATRIX SPOT            
          holdmat <- fnc$get()
          tryCatch(invmat <- solve(holdmat, ...),
                   error = function(e){
                                      message("an error\n",e)
                                      },
                   warning = function(w)
                                      {
                                      message("a warning\n",w)
                                      },
                   finally =
                            {
                            fnc$setinverse(invmat)
                            message("inverse matrix made")
                            return(invmat)
                            }
                  )

          }         
}

