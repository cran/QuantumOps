#' @export
many <- function(gate,n,...){	
	g <- gate
	for(j in 2:n)
		g <- tensor(g,gate)	

	ket <- list(...)
	if(length(ket) == 0){
		g
	} else{
		g %*% ket[[1]]
	}	
}
