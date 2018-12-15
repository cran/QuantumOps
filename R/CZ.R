#' @export
CZ <- function(...){
	CZ <- mm(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,-1)
	ket <- list(...)
	if(length(ket) == 0){
		CZ
	} else {
		CZ %*% ket[[1]]
	}
}
