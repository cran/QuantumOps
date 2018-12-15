#' @export
I <- function(...){
	I <- matrix(c(1,0,0,1),nrow=2,ncol=2)
	ket <- list(...)
	if(length(ket) == 0){
		I
	} else {
		ket[[1]]	#I gate does not change ket
	}
}
