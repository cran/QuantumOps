#' @export
U <- function(...){
	arguments <- list(...)
	arglength <- length(arguments)
	operator <- arguments[[1]]					#first argument must be operator
	if(arglength > 2){
		for( j in 2:(arglength-1) )				#build total operator w/ tensor
			operator <- tensor(operator,arguments[[j]])
	}
	if(length(arguments[[arglength]][1,]) == 1){
		operator %*% arguments[[arglength]]		#last arg is ket, so apply gate
	} else{										#last arg is operator
		if(arglength > 1){
			operator <- tensor(operator,arguments[[arglength]]) #do last tensor if needed
		}
		operator											#return operator
	}														
}
