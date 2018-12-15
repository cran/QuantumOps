#' @export
tensor <- function(...){
	#as.matrix(outer(k1,k2))
	input <- list(...)
	k <- input[[1]]
	for( j in 2:length(input))
		k <- kronecker(k,input[[j]])
	if(length(input[[1]][1,]) == 1)			#if vectors, not matrices
		k <- k/sqrt(sum(abs(k)^2))		#if original kets not normalized
	k								#normalize
}
