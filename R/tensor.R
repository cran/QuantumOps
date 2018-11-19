#' @export
tensor <- function(k1,k2){
	#as.matrix(outer(k1,k2))
	k <- kronecker(k1,k2)
	if(length(k1[1,]) == 1)			#if vectors, not matrices
		k <- k/sqrt(sum(abs(k)^2))		#if original kets not normalized
	k								#normalize
}
