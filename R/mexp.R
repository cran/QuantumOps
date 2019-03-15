
mexp <- function(m,a,limit=1000){		#get matrix exponential of matrix m
	i <- complex(1,0,1)
	eX <- I(dim(m)[1])				#starts as I
	kfact <- 1
	#m <- -i*a*m
	for(k in 2:limit){
		kfact <- kfact * k
		eX <- eX + m/kfact
		print(eX)
		m <- m %*% m
	}
	eX
}
