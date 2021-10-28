print.ABT <- function(x, ...){
	patients <- x$DATA$ID
	pmax <- x$pmax
	longestFromSecond <- x$longestFromSecond
	shortestFromSecond <- x$shortestFromSecond
	cat("  An ABT class object\n")
	cat(paste("    Number of patients: ", length(patients), "\n", sep = ""))
	if(length(patients) > 3){
		cat(paste("        ", paste(patients[1:3], collapse = ", "), ", ...\n", sep = ""))
	}
	if(length(patients) <= 3){
		cat(paste("        ", paste(patients[1:length(patients)], collapse = ", "), "\n", sep = ""))
	}
	cat(paste("    pmax:               ", pmax, "\n", sep = ""))
	cat(paste("    longestFromSecond:  ", longestFromSecond, " (", longestFromSecond/30, " months)", "\n", sep = ""))
	cat(paste("    shortestFromSecond: ", shortestFromSecond, " (", shortestFromSecond/30, " months)", "\n", sep = ""))
	cat(paste("    Attrib:\n", sep = ""))
	
	for(a in 1:length(x$Attrib)){
		if(names(x$Attrib)[a] != "Age"){
			cat("     ",paste(names(x$Attrib)[a], ":\n", sep = ""))
			for(i in 1:length(x$Attrib[[a]])){
				if(i == 1)	cat("       ", paste(names(x$Attrib[[a]][i]), "=", 
								as.character(x$Attrib[[a]][i]), sep = ""))
				if(i != 1)	cat(", ", paste(names(x$Attrib[[a]][i]), "=", 
								as.character(x$Attrib[[a]][i]), sep = ""))
				if(i == length(x$Attrib[[a]]))	cat("\n")
			}
		}
		if(names(x$Attrib)[a] == "Age"){
			cat("     ",paste(names(x$Attrib)[a], ":\n", sep = ""))
			temp <- summary(x$Attrib[[a]])
			cat(paste("        ", "Min:    ", temp[1], "\n", sep = ""))
			cat(paste("        ", "1st Q:  ", temp[2], "\n", sep = ""))
			cat(paste("        ", "Median: ", temp[3], "\n", sep = ""))
			cat(paste("        ", "Mean:   ", temp[4], "\n", sep = ""))
			cat(paste("        ", "3rd Q:  ", temp[5], "\n", sep = ""))
			cat(paste("        ", "Max:    ", temp[6], "\n", sep = ""))
		}		
	}

}
