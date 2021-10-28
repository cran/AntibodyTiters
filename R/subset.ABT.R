subset.ABT <- function(x, ...){
	DATA <- x$DATA
	inData <- subset(DATA, ...)

	additional.colnames <- colnames(inData)[10:ncol(inData)]
	point.index <- grep(pattern = "point", additional.colnames)
	yyyymmdd.index <- grep(pattern = "_yyyymmdd", additional.colnames)
	score.index <- grep(pattern = "_score", additional.colnames)
	
	additional.DF <- data.frame(name = additional.colnames)
	additional.DF$noPoint <- FALSE
	additional.DF$noPoint[grep(pattern = "point", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$noM1 <- FALSE
	additional.DF$noM1[grep(pattern = "M1_", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$noM2 <- FALSE
	additional.DF$noM2[grep(pattern = "M2_", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$noM3 <- FALSE
	additional.DF$noM3[grep(pattern = "M3_", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$noM4 <- FALSE
	additional.DF$noM4[grep(pattern = "M4_", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$noM6 <- FALSE
	additional.DF$noM6[grep(pattern = "M6_", additional.colnames, invert = TRUE)] <- TRUE
	additional.DF$attrib <- FALSE
	additional.DF$attrib[rowSums(additional.DF[,2:7]) == 6] <- TRUE
	attrib.index <- which(additional.DF$attrib == TRUE)
	
	attribColnames <- additional.colnames[attrib.index]
	Attrib <- list()
	for(a in 1:length(attribColnames)){
		if(is.numeric(inData[[attribColnames[a]]]) == FALSE & is.integer(inData[[attribColnames[a]]]) == FALSE){
			colNames <- as.character(unique(inData[[attribColnames[a]]]))
			for(c in 1:length(colNames)){
				if(is.na(colNames[c]) == FALSE){
					Attrib[[attribColnames[a]]][colNames[c]] <- 
						length(which(inData[[attribColnames[a]]] == unique(inData[[attribColnames[a]]])[c]))
				}
				if(is.na(colNames[c]) == TRUE){
					Attrib[[attribColnames[a]]][colNames[c]] <- 
						length(which(is.na(inData[[attribColnames[a]]]) == TRUE))
				}
			}
		}
		if(is.numeric(inData[[attribColnames[a]]]) == TRUE | is.integer(inData[[attribColnames[a]]]) == TRUE){
			# Attrib[[attribColnames[a]]] <- c(min(inData[[attribColnames[a]]]), max(inData[[attribColnames[a]]]))
			Attrib[[attribColnames[a]]] <- inData[[attribColnames[a]]]
		}		
	}
	
	pmax <- x$pmax
	longestFromSecond <- x$longestFromSecond
	shortestFromSecond <- x$shortestFromSecond
			
	inData <- list(DATA = inData, pmax = pmax, 
			longestFromSecond = longestFromSecond, 
			shortestFromSecond = shortestFromSecond, 
			Attrib = Attrib)
	class(inData) <- "ABT"
	return(inData)
}
