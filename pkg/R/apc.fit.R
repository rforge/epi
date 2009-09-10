apc.fit <-
function( data,
             A,
             P,
             D,
             Y,
         ref.c,
         ref.p,
          dist = c("poisson","binomial"),
         model = c("ns","bs","ls","factor"),
       dr.extr = c("weighted","Holford"),
          parm = c("ACP","APC","AdCP","AdPC","Ad-P-C","Ad-C-P","AC-P","AP-C"),
          npar = c( A=5, P=5, C=5 ),
         scale = 1,
         alpha = 0.05,
     print.AOV = TRUE
         )
{
# Get the arguments
model <- match.arg( model )
drtyp <- match.arg( dr.extr )
parm  <- toupper( match.arg( parm ) )

# Check that data has the variables A, P, D and Y
if( !missing( data ) )
  { if( length( match( c("A","P","D","Y"), names( data ) ) ) != 4 )
    stop( "Data frame ", deparse( substitute( data ) ),
          " has columns:\n", names( data ),
          "\nmust have variables:\n",
          "A (age), P (period), D (cases) and Y (person-years)" )
    # Take only the complete cases
    data <- data[,c("A","P","D","Y")]
    data <- data[complete.cases(data),]
    A <- data$A
    P <- data$P
    D <- data$D
    Y <- data$Y
  }
# or that they have been supplied separately and in the same length
else
 { nm <- logical(4)
    nm[1] <- missing( A )
    nm[2] <- missing( P )
    nm[3] <- missing( D )
    nm[4] <- missing( Y )
    if( any( nm ) ) stop( "Variable", if( sum(nm)>1 ) "s",
                          paste( c(" A"," P"," D"," Y")[nm], collapse="," ),
                          " missing from input" )
    if( diff( range( lv <- sapply( list(A=A,P=P,D=D,Y=Y), length ) ) ) != 0 )
      {
        stop( "\nLengths of variables (",
              paste( paste( names( lv ), lv, sep=":" ), collapse=", " ),
              ") are not the same." )
      }
   }
# So presently we have variable A, P, D and Y in the local workspace.

# Define reference period and cohort if not supplied
# Time-saving utility courtesy of Mike Murphy
med <- function(x,y)
{
a <- y
names(a) <- x
return(as.numeric(names(a[cumsum(a)/sum(a)>.5][1])))
}
p0 <- ifelse( missing( ref.p ), med( P  , D), ref.p )
c0 <- ifelse( missing( ref.c ), med( P-A, D), ref.c )

# Redefine to logicals for whether a reference was requested or not
ref.p <- !missing( ref.p )
ref.c <- !missing( ref.c )

# Fix names and no. parameters if only one supplied
if( is.list( npar ) & length( npar ) < 3 ) stop( "npar as a list should have length 3!" )
if( !is.list( npar ) ) npar <- rep( npar, 3 )[1:3]
if( is.null( names( npar ) ) ) names( npar ) <- c("A","P","C")

# Text for the c.i. labelling
lu <- paste( formatC( c(alpha/2,1-alpha/2)*100, format="f", digits=1 ), "%", sep="" )

#----------------------------------------------------------------------
# Define useful matrix functions to manipulate the model matrix.

proj.ip <-
function( X, M, orth = FALSE, weight=rep(1,nrow(X)) )
# Generate the projection of M on span(X) w.r.t the inner
# product <x|y>=sum( x*w*y).
# ( Stolen from PD, modified from stats:::proj.matrix )
# Avoids computing the entire projection matrix
#   X %*% inverse( X'WX ) %*% (XW)'  by first computing
#         inverse( X'WX ) %*% (XW)'M
# (which is (p x p) %*% (p x n) %*% (n x k), i.e. (p x k) )
# and then premultiplying X (n x p) hence avoiding making
# a n x n matrix underway (note that n is large, p is small).
# Note multiplication by W (diagional matrix) is done by
# vector multiplication using the recycling facility of R.
{
  if( nrow(X) != length(weight) )
      stop( "Dimension of space and length of weights differ!" )
  if( nrow(X) != nrow(M) )
      stop( "Dimension of space and rownumber of model matrix differ!" )
  Pp <- solve( crossprod( X * sqrt(weight) ), t( X * weight ) ) %*% M
  PM <- X %*% Pp
  if (orth) PM <- M - PM
  else PM
}

Thin.col <-
function ( X, tol = 1e-06)
# Function to remove lin. dep. columns from a matrix
# (stolen from PD, existed at some time as stats:::Thin.col )
{
  QR <- qr(X, tol = tol, LAPACK = FALSE)
  X[, QR$pivot[seq(length = QR$rank)], drop = FALSE]
}

detrend <-
function( M, t, weight=rep(1,nrow(M)) )
{
# Function to detrend a matrix using the weighted inner product.
  Thin.col( proj.ip( cbind( 1, t ), M , orth = TRUE, weight = weight ) )
}
# End of matrix functions
#----------------------------------------------------------------------

# Generate model matrix for different cases of "model"

# Model definition supplied in a list as functions for each effect
if( is.list( model ) )
  {
  # Are all list elements functions?
  if( !all( sapply( model, is.function ) ) )
      stop( "'model' is a list, but not all elements are functions as they should be." )
  # Is there three elements in list?
  if( ( lmod <- length( model ) ) < 3 )
      stop( "'model' is a list, with", lmod, "elements, it should have three." )
  # Set up the model matrix using the functions supplied
  if( is.null( names( model ) ) ) names( model ) <- c("A","P","C")
  MA <- model[["A"]]( A )
  MP <- model[["P"]]( P )
  MC <- model[["C"]]( P-A )
  # The reference rows if required later
  Rp <- model[["P"]]( p0 )
  Rc <- model[["C"]]( c0 )
  }
  # End of "model" as a list

# Model defined by character argument:
else
  {
  # Factor model
  if( model == "factor" )
    {
    MA <- model.matrix( ~ factor( A ) - 1 )
    MP <- model.matrix( ~ factor( P ) - 1 )
    MC <- model.matrix( ~ factor( P-A ) - 1 )
    # The reference rows if required later
    Rp <- MP[ abs( P   - p0 ) == min( abs( P   - p0 ) ),,drop=FALSE][1,]
    Rc <- MC[ abs( P-A - c0 ) == min( abs( P-A - c0 ) ),,drop=FALSE][1,]
    }
  # End of factor model

  # Natural splines
  if( model == "ns" )
    {
    require( splines )
    # Are knots supplied in a list?
    knl <- is.list( npar )
    if( knl ) nk <- sapply( npar, length )
    MA <- if( knl ) ns( A  , knots=npar[["A"]][-c(1,nk[1])], Bo=npar[["A"]][c(1,nk[1])] )
               else ns( A  ,    df=npar[["A"]] )
    MP <- if( knl ) ns( P  , knots=npar[["P"]][-c(1,nk[2])], Bo=npar[["P"]][c(1,nk[2])] )
               else ns( P  ,    df=npar[["P"]] )
    MC <- if( knl ) ns( P-A, knots=npar[["C"]][-c(1,nk[3])], Bo=npar[["C"]][c(1,nk[3])] )
               else ns( P-A,    df=npar[["C"]] )
    # The reference rows if required later
    Rp <- ns( p0, knots=attr( MP, "knots"), Boundary.knots=attr( MP, "Boundary.knots" ) )
    Rc <- ns( c0, knots=attr( MC, "knots"), Boundary.knots=attr( MC, "Boundary.knots" ) )
    # The knots used, to be output with the results
    Knots <- list( Age = sort( c(attr( MA, "knots"),attr( MA, "Boundary.knots" )) ),
                   Per = sort( c(attr( MP, "knots"),attr( MP, "Boundary.knots" )) ),
                   Coh = sort( c(attr( MC, "knots"),attr( MC, "Boundary.knots" )) ) )
    }
  # End of natural splines

  # B-splines
  if( model %in% c("bs","ls") )
    {
    # Set the degree of the polynomium
    deg <- switch( model, "ls"=1, "bs"=3 )
    require( splines )
    # Are knots supplied in a list?
    knl <- is.list( npar )
    if( knl ) nk <- sapply( npar, length )
    MA <- if( knl ) bs( A, knots=npar["A"][-c(1,nk[1])],
                           Bo=npar["A"][c(1,nk[1])], degree=deg )
               else bs( A, df=npar["A"], degree=deg )
    MP <- if( knl ) bs( P, knots=npar["P"][-c(1,nk[2])],
                           Bo=npar["P"][c(1,nk[2])], degree=deg )
               else bs( P, df=npar["P"], degree=deg )
    MC <- if( knl ) bs( P-A, knots=npar["C"][-c(1,nk[3])],
                             Bo=npar["C"][c(1,nk[3])], degree=deg )
               else bs( P-A, df=npar["C"], degree=deg )
    # The reference rows if required later
    Rp <- bs( p0, knots=attr( MP, "knots"),
         Boundary.knots=attr( MP, "Boundary.knots" ),
                 degree=attr( MP, "degree" ) )
    Rc <- bs( c0, knots=attr( MC, "knots"),
         Boundary.knots=attr( MC, "Boundary.knots" ),
                 degree=attr( MC, "degree" ) )
    # The knots used, to be output with the results
    Knots <- list( Age = sort( c(attr( MA, "knots"),attr( MA, "Boundary.knots" )) ),
                   Per = sort( c(attr( MP, "knots"),attr( MP, "Boundary.knots" )) ),
                   Coh = sort( c(attr( MC, "knots"),attr( MC, "Boundary.knots" )) ) )
    }
  # End of B-splines.

}
# End of defining the three chunks of the model matrix that makes up the
# model specification.

# Fit the base model
if( tolower(substr(dist,1,2)) == "po" )
    {
    m.APC <- glm( D ~ MA + I(P-p0) + MP + MC,
                  offset =log( Y ), family = poisson )
    Dist <- "Poisson with log(Y) offset"
    }
if( tolower(substr(dist,1,3)) %in% c("bin") )
    {
    m.APC <- glm( cbind( D, Y-D ) ~ MA + I(P-p0) + MP + MC,
                  family = binomial )
    Dist <- "Binomial regression (logistic) of D/Y"
    }

# Comparison of the 5 classical models:
#--------------------------------------
m.AP  <- update( m.APC, . ~ . - MC )
m.AC  <- update( m.APC, . ~ . - MP )
m.Ad  <- update( m.AP , . ~ . - MP )
m.A   <- update( m.Ad , . ~ . - I(P-p0) )
m.0   <- update( m.A  , . ~ . - MA )
AOV   <- anova( m.A, m.Ad, m.AC, m.APC, m.AP, m.Ad, test="Chisq" )
# Change the header and row-names to readable form
attr( AOV, "heading") <- "\nAnalysis of deviance for Age-Period-Cohort model\n"
attr( AOV, "row.names") <- c("Age","Age-drift",
                                   "Age-Cohort",
                                   "Age-Period-Cohort",
                                   "Age-Period",
                                   "Age-drift")

# Now for the parametrizations:
# -----------------------------

# For model reporting we need the unique values of A, P and C, and the
# locations of any, but in this case the first occurrences in the dataset
A.pt  <- unique( A )
A.pos <- match( A.pt, A )
P.pt  <- unique( P )
P.pos <- match( P.pt, P )
C.pt  <- unique( P-A )
C.pos <- match( C.pt, P-A )

# Construct model matrices corresponding to needs
# ------------------------------------------------

# Include the intercept with the age-columns
MA <- cbind( 1, MA )

# Detrend the period and cohort effects according to an
# inner product defined by a set of weights:
if( !mode( drtyp ) %in% c("character","numeric") )
    stop( '"dr.extr" must be of mode "character" or "numeric".\n' )
if( is.character( drtyp ) )
    wt <- if( toupper(substr(drtyp,1,1))=="W") D else rep(1,length(D))
if( is.numeric( drtyp ) )
    wt <- drtyp

# If reference points for period and/or cohort are given, the detrended
# versions are adjusted so that the effects are 0 at the values p0 resp c0.
# Make sure the reference rows are row matrices, so they correspond to rows
# in the model matrices
Rp <- matrix( Rp, nrow=1 )
Rc <- matrix( Rc, nrow=1 )

# In order to have the projection operation performed on the reference points
# we just add these as an extra row, use weight 0 for the inner product
# and remove it after the operation.
xP <- detrend( rbind( Rp, MP ), c(p0,P  ), weight=c(0,wt) )
xC <- detrend( rbind( Rc, MC ), c(c0,P-A), weight=c(0,wt) )

# If reference points were given, adjust effect to be 0 at these points
MPr <- xP[-1,] - ref.p * xP[rep(1,nrow(MP)),]
MCr <- xC[-1,] - ref.c * xC[rep(1,nrow(MC)),]

# Modelfitting and reporting of effects
# -------------------------------------

if( length( grep( "-", parm ) ) == 0 )
  # If "parm" is one of the ML-approaches (no hyphen in the name)
  {
  # Fit the model with model matrices designed so that effects are packed
  # in the right places, and accesible by grep-ing for "MA", "MPr","MCr":
  
  # We must fit a model with explicit drift to get the drift estimate
  if( parm %in% c("ADPC","ADCP","APC","ACP") )
    m.APC <- update( m.0, .~.-1 + MA + I(P-p0) + MPr + MCr )
                  
  # Extract the drift estimates
  drift <- rbind( ci.lin( m.APC, subset="I\\(", Exp=TRUE, alpha=alpha )[,5:7],
                  ci.lin( m.Ad , subset="I\\(", Exp=TRUE, alpha=alpha )[,5:7] )
  rownames( drift ) <- c("APC","A-d")

  # Then we can fit the other models if needed
  if( parm == "ADCP" )
    m.APC <- update( m.0, .~.-1 + MA + I(P-A-c0) + MPr + MCr )
  if( parm == "APC" )
    {
    MPr <- cbind( P  -p0, MPr )
    m.APC <- update( m.0, .~.-1 + MA + MPr + MCr )
    }
  if( parm == "ACP" )
    {
    MCr <- cbind( P-A-c0, MCr )
    m.APC <- update( m.0, .~.-1 + MA + MPr + MCr )
    }

  # Then extract the effects:
  Age <- cbind( Age=A.pt,
                ci.lin( m.APC, subset="MA", ctr.mat=MA[A.pos,],
                        Exp=TRUE, alpha=alpha )[,5:7] )[order(A.pt),]
  Per <- cbind( Per=P.pt,
                ci.lin( m.APC, subset="MPr", ctr.mat=MPr[P.pos,],
                        Exp=TRUE, alpha=alpha )[,5:7] )[order(P.pt),]
  Coh <- cbind( Coh=C.pt,
                ci.lin( m.APC, subset="MCr", ctr.mat=MCr[C.pos,],
                        Exp=TRUE, alpha=alpha )[,5:7] )[order(C.pt),]
  colnames( Age )[-1] <- c("Rate",lu)
  colnames( Per )[-1] <- c("P-RR",lu)
  colnames( Coh )[-1] <- c("C-RR",lu)

  Type <- paste( "ML of APC-model", Dist, ": (", parm, "):\n" )
}
# End of ML-options
else
{ # Sequential approach (if a hyphen IS in the name)

  # Age-drift model always needed
  adc <- update( m.0, .~.-1 + MA + I(P-A-c0) )
  adp <- update( m.0, .~.-1 + MA + I(P  -p0) )
  # The raw drift parameter
  drift <- ci.lin( adc, subset="I\\(", Exp=TRUE )[,5:7,drop=F]
  rownames( drift ) <- "A-d"

  # Expanded model matrices, including intercept and drift
  xP <- cbind( 1, P  -p0, MPr )
  xC <- cbind( 1, P-A-c0, MCr )
  lP <- cbind(    P  -p0, MPr )
  lC <- cbind(    P-A-c0, MCr )
  
  # The four different model options
  if( parm == "AD-C-P" )
    {
    # Fit the two residual models and extract the parameters
    rc <- update( m.0, .~.-1 + xC, offset = predict( adc, type="link" ) )
    rp <- update( m.0, .~.-1 + xP, offset = predict( adc, type="link" ) )
    A.eff <- ci.lin( adc, subset="MA", ctr.mat=MA[A.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    C.eff <- ci.lin(  rc, subset="xC", ctr.mat=xC[C.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    P.eff <- ci.lin(  rp, subset="xP", ctr.mat=xP[P.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    }
    # End of "Ad-C-P"
  else
  if( parm == "AD-P-C" )
    {
    # Fit the two residual models in other sequence and extract the parameters
    rp <- update( m.0, .~.-1 + xP, offset = predict( adp, type="link" ) )
    rc <- update( m.0, .~.-1 + xC, offset = predict(  rp, type="link" ) )
    A.eff <- ci.lin( adp, subset="MA", ctr.mat=MA[A.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    P.eff <- ci.lin(  rp, subset="xP", ctr.mat=xP[P.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    C.eff <- ci.lin(  rc, subset="xC", ctr.mat=xC[C.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    }
    # End of "Ad-P-C"
  else
  if( parm == "AC-P" )
    {
    ac <- update( m.0, .~.-1 + MA + lC )
    rp <- update( m.0, .~.-1 + xP, offset = predict( ac, type="link" ) )
    A.eff <- ci.lin( ac, subset="MA", ctr.mat=MA[A.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    C.eff <- ci.lin( ac, subset="lC", ctr.mat=lC[C.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    P.eff <- ci.lin( rp, subset="xP", ctr.mat=xP[P.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    }
    # End of "AC-P"
  else
  if( parm == "AP-C" )
    {
    ap <- update( m.0, .~.-1 + MA + lP )
    rc <- update( m.0, .~.-1 + xC, offset = predict( ap, type="link" ) )
    A.eff <- ci.lin( ap, subset="MA", ctr.mat=MA[A.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    P.eff <- ci.lin( ap, subset="lP", ctr.mat=lP[P.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    C.eff <- ci.lin( rc, subset="xC", ctr.mat=xC[C.pos,], Exp=TRUE, alpha=alpha )[,5:7]
    }
    # End of "AP-C"

  # Pack up the effects in correct order and with nice names
  Age <- cbind( Age=A.pt, A.eff )[order(A.pt),]
  Per <- cbind( Per=P.pt, P.eff )[order(P.pt),]
  Coh <- cbind( Cph=C.pt, C.eff )[order(C.pt),]
  colnames( Age )[-1] <- c("A.eff",lu)
  colnames( Per )[-1] <- c("P.eff",lu)
  colnames( Coh )[-1] <- c("C.eff",lu)

  Type <- paste( "Sequential modelling", Dist, ": (", parm, "):\n" )
}
# end of sequential approach

# Pack up the entire set of results
res <- list( Type=Type,
             Age=Age, Per=Per, Coh=Coh, Drift=drift,
             Ref=c(Per=if( ref.p ) p0 else NA,
                   Coh=if( ref.c ) c0 else NA),
             Anova=AOV )
if( model %in% c("ns","bs") ) res <- c( res, list( Knots=Knots) )
res$Age[,-1] <- res$Age[,-1] * scale
if( print.AOV ) { print( res$Type )
                  print( res$Anova ) }
class( res ) <- "apc"
invisible( res )
}