tab <-
function (x, ...)
UseMethod("tab")

tab.Lexis <-
function( x, simplify=TRUE, ... )
{
tr <- trans <- with( x, table(lex.Cst,lex.Xst) )
for( i in intersect(rownames(trans),colnames(trans)) ) tr[i,i] <- 0
trans <- addmargins(trans)
tr    <- addmargins(tr)
tr    <- tr[,ncol(tr)]
pyrs  <- with( x, addmargins( tapply(lex.dur,lex.Cst,sum,na.rm=TRUE),
                              FUN=function(x) sum(x,na.rm=TRUE) ) )
res <- cbind( trans, tr, pyrs )
colnames( res )[ncol(res)-1:0] <- c(" D(events)","Y(pyrs)")
names( dimnames( res ) ) <- c("From","\nStates\n     To\n     (records)")
if( simplify ) res <- res[!is.na(pyrs),]
if( nrow( res )==2 ) res <- res[1,,drop=FALSE]
res
}
