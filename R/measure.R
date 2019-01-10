#bit-wise mod 2 add two integers
#' @export
measure <- function(...){
	input <- list(...)
	if( length(input) == 1){		#input is just a ket, measure all qubits
		qstate <- unlist(input[[1]])
		p <- probs(as.complex(qstate))
		m <- sample( seq(1,length(qstate),by=1), size=1, prob=p )
		amplitudes <- rep(0,length(qstate))
		amplitudes[m] <- 1
		print(amplitudes)
		do.call(ket, as.list(amplitudes) )
	}
}
