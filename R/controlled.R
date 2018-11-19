#make controlled version of input gate
#' @export
controlled <- function(gate){
	g <- mm(1,0,0,0, 0,1,0,0 , 0,0,0,0,0,0,0,0)
	g[3,3] <- gate[1,1]
	g[3,4] <- gate[1,2]
	g[4,3] <- gate[2,1]
	g[4,4] <- gate[2,2]
	g
}
