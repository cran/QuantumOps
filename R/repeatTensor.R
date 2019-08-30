#' @export
repeatTensor <- function(g,n){
	G <- g
	if(n > 1){
		for(t in 2:n)
			G <- tensor(G,g)
	}
	G
}

