#' @export
Yg <- function(ket){
	i <- complex(1,0,1)	
	Y <- matrix(c(0,i,-i,0),nrow=2,ncol=2)
	k <- Y %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	#} else{
	#	k <- k*Conj(k[2,1])
	#}
	ket(k[1,1],k[2,1])
}
