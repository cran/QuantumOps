#' @export
X <- function(...){
	X <- matrix(c(0,1,1,0),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		X
	} else {
		k <- X %*% ket[[1]]
		if(abs(k[1,1]) != 0)
			k <- k/k[1,1]
		ket(k[1,1],k[2,1])		
	}
}
