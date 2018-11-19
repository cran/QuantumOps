#' @export
CXg <- function(ket){
	CX <- mm(1,0,0,0, 0,1,0,0, 0,0,0,1, 0,0,1,0)
	CX %*% ket
}
