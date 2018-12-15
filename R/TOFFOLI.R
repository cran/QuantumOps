#Equivalent to CCNOT
#' @export
TOFFOLI <- function(...){
	TOFFOLI <- matrix(c(1,0,0,0,0,0,0,0,
					0,1,0,0,0,0,0,0,
					0,0,1,0,0,0,0,0,
					0,0,0,1,0,0,0,0,
					0,0,0,0,1,0,0,0,
					0,0,0,0,0,1,0,0,
					0,0,0,0,0,0,0,1,
					0,0,0,0,0,0,1,0),nrow=8,ncol=8)
	ket <- list(...)
	if(length(ket) == 0){
		TOFFOLI
	} else {
		TOFFOLI %*% ket[[1]]
	}
}
