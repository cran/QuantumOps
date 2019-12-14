
opDM <- function(V,G){
	G %*% V %*% adjoint(G)
}
