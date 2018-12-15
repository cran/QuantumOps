#' @export
H <- function(...){
	H <- 1/2^.5 * matrix(c(1,1,1,-1),nrow=2,ncol=2)		#H gate
	ket <- list(...)
	if(length(ket) == 0){
		H
	} else{	
		k <- H %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])
	}
}


