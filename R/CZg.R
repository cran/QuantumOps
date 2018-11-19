#' @export
CZg <- function(ket){
	CZ <- mm(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,-1)
	CZ %*% ket
}
