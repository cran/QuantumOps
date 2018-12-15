#' @export
R <- function(theta,...){
	i <- complex(1,0,1)	
	R <- matrix(c(1,0,0,exp(i*theta)),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		R
	} else {
		k <- R %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])	
	}
}
