#' @export
CY <- function(...){
	i <- complex(1,0,1)	
	CY <- mm(1,0,0,0, 0,1,0,0, 0,0,0,i, 0,0,-i,0)
	ket <- list(...)
	if(length(ket) == 0){
		CY
	} else{
		CY %*% ket[[1]]
	}
}
