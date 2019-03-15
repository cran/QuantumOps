#' @export
QAOA_example <- function(case=1){
	P <- 20
	g <- pi/(1*P)
	b <- pi/(2*P)
	if(case==1){
		c1 <- c(1,-1)
		c2 <- c(1,1)
		clauses <- rbind(c1,c2)
	} else{
		c1 <- c(1,1,0,0)
		c2 <- c(1,1,0,1)
		c3 <- c(1,1,-1,-1)
		clauses <- rbind(c1,c2,c3)
	}

	v <- QAOA(clauses,gamma=g,beta=b,p=P,displayProgress=TRUE)
}

