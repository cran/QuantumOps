#' @export
Xg <- function(ket){
	X <- matrix(c(0,1,1,0),nrow=2,ncol=2)
	k <- X %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	ket(k[1,1],k[2,1])
}
