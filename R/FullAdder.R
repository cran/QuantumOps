


FullAdder <- function(n=4,cin=0,a=1,b=2,cout=3){
	g <- list()
	#Toffoli a,b - ci
	g <- c(g,		TOFFOLI(byCycle=TRUE,n=n,cQubits=c(a,b),tQubit=cout) )
	g <- c(g,list(	controlled(X(),n=n,cQubits=a,tQubit=b) ))
	g <- c(g,		TOFFOLI(byCycle=TRUE,n=n,cQubits=c(cin,b),tQubit=cout) )
	g <- c(g,list(	controlled(X(),n=n,cQubits=cin,tQubit=b) ))
	g
}
