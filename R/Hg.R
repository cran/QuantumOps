#' @export
Hg <- function(ket){
	k <- H %*% ket
	if(abs(k[1,1]) != 0)
		k <- k/k[1,1]
	ket(k[1,1],k[2,1])
}
