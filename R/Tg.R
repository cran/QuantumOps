#R(pi/4)
#' @export
Tg <- function(ket){
	i <- complex(1,0,1)
	T <- matrix(c(1,0,0,exp(i*pi/4)),nrow=2,ncol=2)
	k <- T %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	ket(k[1,1],k[2,1])
}
