groupAttribABT <- function(objName = "inData", sourceAttrib = "Age", 
					newAttribName = "AgeGroup", groupNames = c("young", "middle", "elderly"),
					groupLimmits = list(c(0, 39), c(40, 64), c(65, 200))){
	x <- get(objName)
	DATA <- x$DATA
	inAttrib <- x$Attrib
	
	if(length(which(names(inAttrib) == sourceAttrib)) == 0){
		stop(paste("sourceAttrib (", sourceAttrib, ") must be included in the Attrib of ", objName, ".\n", sep = ""))
	}
	if(is.numeric(inAttrib[[sourceAttrib]]) == FALSE & is.integer(inAttrib[[sourceAttrib]]) == FALSE){
		stop(paste("sourceAttrib (", sourceAttrib, ") in the Attrib of ", objName, 
					" must be an integer or numeric vector", ".\n", sep = ""))
	}
	if(length(groupNames) != length(groupLimmits)){
		stop(paste("Length of groupNames must be the same as that of groupLimmits.\n"))
	}
	
	names(groupLimmits) <- groupNames
	DATA[[newAttribName]] <- as.character(NA)
	for(g in 1:length(groupLimmits)){
		groupName <- groupNames[g]
		lowerLimmit <- groupLimmits[[groupName]][1]
		upperLimmit <- groupLimmits[[groupName]][2]
		trueIndex <- DATA[[sourceAttrib]] >= lowerLimmit & DATA[[sourceAttrib]] <= upperLimmit
		DATA[[newAttribName]][trueIndex] <- groupName
	}
	
	inData <- DATA
	
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
				Attrib[[attribColnames[a]]][colNames[c]] <- 
						length(which(inData[[attribColnames[a]]] == unique(inData[[attribColnames[a]]])[c]))
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
