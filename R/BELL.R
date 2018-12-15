#Bell gate on 4-dimensional ket
#' @export
BELL <- function(...){
	CNOT <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,0,1, 0,0,1,0),nrow=4,ncol=4)
	H <- 1/2^.5 * matrix(c(1,1,1,-1),nrow=2,ncol=2)		#H gate
	I <- matrix(c(1,0,0,1),nrow=2,ncol=2)
	BELL <- CNOT %*% tensor(H,I)

	ket <- list(...)
	if(length(ket) == 0){
		BELL
	} else {
		BELL %*% ket[[1]]
	}
}
