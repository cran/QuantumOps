

randomConnectionMatrix <- function(nNodes,nEdges){
	maxEdges <- nNodes*(nNodes-1)/2									#Undirected graph has n*(n-1)/2 edges
	if(nEdges > maxEdges)
		print("Not enough nodes for number of requested edges")

	connectionMatrix <- matrix(rep(0,nNodes^2),nrow=nNodes)			#Empty connection matrix
	pedges <- seq(1,maxEdges+nNodes)									#List of indices at "top right" of matrix
	diagonal <- seq(1,nNodes^2,by=nNodes+1)
	pedges <- pedges[ !pedges %in% diagonal ]							#Get all edges that are not on diagonal

	edges <- sample( seq(1, maxEdges), size=nEdges, replace=FALSE)		#get random indices for edges
	for(j in 1:nEdges){
		n <- pedges[ edges[j] ]
		r <- floor(n/nNodes) + 1
		c <- (n %% nNodes) + 1
		connectionMatrix[r,c] <- 1
		connectionMatrix[c,r] <- 1
	}
	connectionMatrix
}
