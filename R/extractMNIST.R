
#' @export
extractMNIST <- function(data,labels,s,centercrop=TRUE){
	if(centercrop)
		nCols <- 256
	else
		nCols <- 28*28

	#get data ready
	imfile <- file(data,"rb")
	d <- readBin(imfile,integer(),4,endian="big")
	print(paste("There are",d[2],"images that each have",d[3],"rows and",d[4],"columns"))

	lbfile <- file(labels,"rb")
	d <- readBin(lbfile,integer(),2,endian="big")
	print(paste("Label set has",d[2],"labels"))

	x <- matrix(,nrow=s,ncol=nCols)
	for(j in 1:s){
		t <- readBin(imfile,integer(),28*28,endian="big",size=1,signed=FALSE)
		if(centercrop){
			t <- matrix(t,nrow=28,byrow=TRUE)
			t <- t[7:22, 7:22]
		}
		x[j,] <- as.integer(t)
	}

	y <- rep(NA,s)
	for(j in 1:s){
		t <- readBin(lbfile,integer(),1,endian="big",size=1,signed=FALSE)
		y[j] <- as.integer(t)
	}
	list(x,y)
}

