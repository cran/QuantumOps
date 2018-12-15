#' @export
CX <- function(...){
	CX <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,0,1, 0,0,1,0),nrow=4,ncol=4)
	ket <- list(...)
	if(length(ket) == 0){
		CX
	} else{
		CX %*% ket[[1]]
	}	
}
