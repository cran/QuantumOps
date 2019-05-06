
#' @export
Steane <- function(v){
	if(length(v) > 2)
		print("Warning: Cannot create Steane version of ket with more than one qubit")

	#Create Steane encoded |0> and |1>
	# |0> = |0000000> + |1010101> + |0110011> + |1100110> +|0001111> + |1011010> + |0111100> + |1101001>
	SteaneZeroAmplitudes <- c(0,85,51,102,15,90,60,105)
	# |1> = |1111111> + |0101010> + |1001100> + |0011001> +|1110000> + |0100101> + |1000011> + |0010110>
	SteaneOneAmplitudes <- c(127,42,76,25,112,37,67,22)

	SteaneZero <- intket(SteaneZeroAmplitudes,7)
	SteaneOne <- intket(SteaneOneAmplitudes,7)

	do.call(ket,as.list(v[1,1]*SteaneZero + v[2,1]*SteaneOne))			#Assign amplitudes of |0> and |1> in original to encoded versions
}
