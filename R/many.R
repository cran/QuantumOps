#' @export
many <- function(gate,n,...){	
	g <- gate
	if(n > 1)
		for(j in 2:n)
			g <- tensor(g,gate)	

	ket <- list(...)
	if(length(ket) == 0){
		g
	} else{
		g %*% ket[[1]]
	}	
}
