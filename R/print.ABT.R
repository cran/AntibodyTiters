print.ABT <- function(x, ...){
	patients <- x$DATA$ID
	pmax <- x$pmax
	longestFromSecond <- x$longestFromSecond
	shortestFromSecond <- x$shortestFromSecond
	cat("An ABT class object\n")
	cat(paste("  Number of patients: ", length(patients), "\n", sep = ""))
	if(length(patients) > 3){
		cat(paste("      ", paste(patients[1:3], collapse = ", "), ", ...\n", sep = ""))
	}
	if(length(patients) <= 3){
		cat(paste("      ", paste(patients[1:length(patients)], collapse = ", "), "\n", sep = ""))
	}
	cat(paste("  pmax:               ", pmax, "\n", sep = ""))
	cat(paste("  longestFromSecond:  ", longestFromSecond, "\n", sep = ""))
	cat(paste("  shortestFromSecond: ", shortestFromSecond, "\n", sep = ""))
}
