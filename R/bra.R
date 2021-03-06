#' @export
bra <- function(...){
	coeff <- as.complex(list(...))
	if(Im(coeff[1]) > 0)
		coeff <- coeff / coeff[1]
	coeff <- coeff/sqrt(sum(abs(coeff)^2))
	if(Re(coeff[1]) < 0)
		coeff <- coeff * -1
	matrix(coeff,nrow=1)
}
