#generate a (column vector) with arbitrary no of states
#' @export
colv <- function(...){
	coeff <- as.complex(list(...))
	matrix(coeff,ncol=1)
}
