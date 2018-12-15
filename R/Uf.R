
#' @export
Uf <- function(fun,n,m){
	Um <- matrix(rep(0,(2^(n+m))^2),nrow=2^(m+n))	#2^(n+m) x 2^(n+m)
	f <- match.fun(fun)
	#Iterate over each 2^n inputs
	for(x in 0:(2^n-1)){
		#compute f(x), which will determine the matrix along the diagonal
		b <- as.character(rev(intToBits(f(x))))
		l <- length(b)
		if(b[l-m+1] == "00"){
			g <- I()
		} else{
			g <- X()
		}
		if(m >= 2){
			for(j in (l-m+2):l){
				if(b[j] == "00"){
					g <- tensor(g,I())
				} else{
					g <- tensor(g,X())
				}
			}
		}
		Um[ ((2^m)*x+1):((2^m)*(x+1)) , ((2^m)*x+1):((2^m)*(x+1)) ] <- g
	}
	Um
}
