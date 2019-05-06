#' @export
convert_dec2bin <- function(x,len=32){
	b <- as.integer(rev(intToBits(x)))
	if(len < 32)
		b <- b[ (32-len+1):32 ]
	if(len > 32)
		b <- c( rep(0,len-32) , b)
	b
}
