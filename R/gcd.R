gcd <- function(x,y) {
	r <- x%%y;
	return(ifelse(r, gcd(y, r), y))
}
