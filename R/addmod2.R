#bit-wise mod 2 add two integers
#' @export
addmod2 <- function(x,a){
	bx <- as.character(intToBits(x))
	ba <- as.character(intToBits(a))
	s <- 0
	for(j in 1:length(bx)){
		if( (bx[j] == "00" & ba[j] == "01") | (bx[j] == "01" & ba[j] == "00") )
			s <- s + 2^(j-1)
	}
	s
}
