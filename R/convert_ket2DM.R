
convert_ket2DM <- function(v){
	kronecker(v,adjoint(v))
}
