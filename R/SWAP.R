#' @export
SWAP <- function(...){
	Swap <- matrix(c(1,0,0,0, 0,0,1,0, 0,1,0,0, 0,0,0,1),nrow=4,ncol=4)
	ket <- list(...)
	if(length(ket) == 0){
		Swap
	} else {
		k <- Swap %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1],k[3,1],k[4,1])		
	}
}
