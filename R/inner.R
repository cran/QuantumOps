#' @export
inner <- function(w,v){
	(Conj(t(w)) %*% v)[1]			#[1,1] is to take element out of "matrix", it's a scaler
}
