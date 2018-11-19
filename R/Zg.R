#' @export
Zg <- function(ket){
	Z <- matrix(c(1,0,0,-1),nrow=2,ncol=2)
	k <- Z %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	ket(k[1,1],k[2,1])
}
