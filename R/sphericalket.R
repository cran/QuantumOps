#' @export
sphericalket <- function(theta,phi){
	i <- complex(1,0,1)
	ket(cos(theta/2),exp(i*phi)*sin(theta/2))		#generate ket from spherical coordinates
}
