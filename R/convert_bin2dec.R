#' @export
convert_bin2dec <- function(b){
	sum( 2^(seq(length(b)-1,0,by=-1))*b )
}
