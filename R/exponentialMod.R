#bit-wise mod 2 add two integers
#' @export
exponentialMod <- function(a,N){
	f <- function(x){
		val <- 1
		for(j in 1:x){
			val <- (val * a) %% N
		}
		val
	}
	f
}



