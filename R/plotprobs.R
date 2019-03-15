#check if matrix is hermitian
#' @export
plotprobs <- function(v,color=rep("Blue",length(v)),customLegend=FALSE,lgNm="",lgCl=""){
	d <- length(v[,1])			#dimension of ket
	l <- ceiling(log(d,base=2))		#number of bits required to specify all dimensions
	s <- rep("",d)
	for(j in 1:d){
		b <- intToBits(j-1)											#get bits of int
		b <- rev(b)													#reverse
		b <- as.character(b)										#make character array
		b <- sapply(strsplit(b, "", fixed = TRUE), `[`, 2)			#remove leading 0's
		bl <- length(b)												#get length of array
		b <- b[(bl-l+1):bl]											#just want last few
		b <- paste(b,collapse="")									#make 1 strings
		s[j] <- paste("|",b,">",sep="")
	}
	amplitudes <- t(probs(v))
	if(!customLegend){
		barplot(amplitudes,main="Absolute Value of Amplitudes",ylab="Probability",names.arg=s,las=2,col=color,beside=TRUE)
	} else{
		par(mar=c(5,3,3,5.5),xpd=TRUE)
		barplot(amplitudes,main="Absolute Value of Amplitudes",ylab="Probability",names.arg=s,las=2,col=color,beside=TRUE)
		legend("right",legend=as.expression(lgNm),fill=lgCl,inset=c(-0.20,0))
	}
}
