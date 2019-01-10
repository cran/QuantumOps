#Print dirac notation of ket
#' @export
dirac <- function(ket){
	d <- length(ket[,1])	#dimension of ket
	s <- ""
	l <- ceiling(log(d,base=2))		#number of bits required to specify all dimensions
	first <- TRUE
	for(x in 1:d){
		if(abs(ket[x,1]) > 0){
			if(first){
				first <- FALSE
			} else{
				s <- paste(s,"+ ")
			}
			#Format ket
			b <- intToBits(x-1)											#get bits of int
			b <- rev(b)													#reverse
			b <- as.character(b)										#make character array
			b <- sapply(strsplit(b, "", fixed = TRUE), `[`, 2)			#remove leading 0's
			bl <- length(b)												#get length of array
			b <- b[(bl-l+1):bl]											#just want last few
			b <- paste(b,collapse="")									#make 1 strings

			#Format coefficient
			istring <- ""
			v <- ket[x,1]
			if(Im(v) < 1e-15){
				v <- Re(ket[x,1])
			} else if(Re(v) < 1e-15){
				v <- Im(ket[x,1])
				istring <- "i"
			}
			s <- paste(s,signif(v,digits=3),istring,"|",b,">",sep="")						#copy into dirac rep
		}
	}
	s
}
