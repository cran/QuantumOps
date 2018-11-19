#' @export
CYg <- function(ket){
	i <- complex(1,0,1)	
	CY <- mm(1,0,0,0, 0,1,0,0, 0,0,0,i, 0,0,-i,0)
	CY %*% ket
}
