#R(pi/2)
#' @export
Sg <- function(ket){
	i <- complex(1,0,1)
	S <- matrix(c(1,0,0,i),nrow=2,ncol=2)
	k <- S %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	ket(k[1,1],k[2,1])
}
