#R(pi/2)
#' @export
S <- function(...){
	i <- complex(1,0,1)
	S <- matrix(c(1,0,0,i),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		S
	} else {
		k <- S %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])
	}
}
