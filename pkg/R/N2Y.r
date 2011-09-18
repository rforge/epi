N2Y <-
function( A, P, N,
          data=NULL,
          return.dfr=TRUE )
{
# Make local versions of variables if a dataframe is supplied
if( !is.null(data) )
  {
  A <- if( !missing(A) ) eval( substitute(A), data, parent.frame() )
       else data$A
  P <- if( !missing(P) ) eval( substitute(P), data, parent.frame() )
       else data$P
  N <- if( !missing(N) ) eval( substitute(N), data, parent.frame() )
       else data$N
  }
# Derive the interval lengths from supplied data
A.int <- unique( diff(sort(unique(A))) )
P.int <- unique( diff(sort(unique(P))) )
# Check if something is fishy
if( length(A.int)!=1 ) stop( "Non-uniform age interval lengths:\n", A.int )
if( length(P.int)!=1 ) stop( "Non-uniform period interval lengths:\n", P.int )
if( A.int!=P.int ) stop( "Unequal age and period interval lengths:\n",
                         "age: ", A.int, ", period: ", P.int )
# Put population prevalence data in a table
# Ntab <- with( N.dk, xtabs( N ~ A+P ) )
Ntab <- xtabs( N ~ A+P )
# Devise a table for the risk times - note one less age and period category
Ydim <- c(dimnames(Ntab),list(wh=c("up","lo")))
# str( Ydim )
Ytab <- array( NA, dim=sapply(Ydim,length),
                   dimnames = Ydim )[-dim(Ntab)[1],-dim(Ntab)[2],]
# str(Ytab)
na <- nrow(Ytab)
np <- ncol(Ytab)
for(a in 1:na) for(p in 1:np)
   {
              Ytab[a,p,"up"] <- Ntab[a  ,p]/3 + Ntab[a+1,p+1]/6
   if( a > 1) Ytab[a,p,"lo"] <- Ntab[a-1,p]/6 + Ntab[a  ,p+1]/3
         else Ytab[a,p,"lo"] <- Ntab[a+1,p+1]/2
   }
# Remember to check the follow-up time
Ytab <- Ytab * A.int
# Convert to a data frame if required (the default)
if( return.dfr )
  {
  # If a dataframe is requires as resturn
  Ytab <- as.data.frame(as.table(Ytab))
  # Retrieve the numerical values of left endpoints of intervals
  Ytab <- transform( Ytab,
                     A = as.numeric(as.character(Ytab[,1])),
                     P = as.numeric(as.character(Ytab[,2])) )
  # Compute the correct midpoints from the supplied data
  Ytab <- transform( Ytab,
                     A = A + A.int*(1+(wh=="up"))/3,
                     P = P + P.int*(2-(wh=="up"))/3,
                     Y = Freq )[,c("A","P","Y")]
  }
Ytab
}