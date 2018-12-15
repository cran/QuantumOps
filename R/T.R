#R(pi/4)
#' @export
T <- function(...){
	i <- complex(1,0,1)
	T <- matrix(c(1,0,0,exp(i*pi/4)),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		T
	} else {
		k <- T %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])	
	}
}
