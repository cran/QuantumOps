#compute distance between to vectors
#' @export
dist <- function(a,b){
	sqrt(sum(abs(b-a)^2))
}
