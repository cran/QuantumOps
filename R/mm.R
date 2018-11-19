#make matrix
#' @export
mm <- function(...){
	numbers <- as.complex(list(...))
	matrix(numbers,nrow=sqrt(length(numbers)))
}
