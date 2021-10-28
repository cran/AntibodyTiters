halfLifeABT <- function(x, output = "list", OutFileName = "Thalf.xlsx"){
	temp <- x$DATA
	pmax <- x$pmax
	IDs <- temp$ID
	shortestFromSecond <- x$shortestFromSecond
	
	DateColNames <- character(length = pmax - 2)
	ScoreColNames <- character(length = pmax - 2)
	DateColIndexs <- integer(length = pmax - 2)
	ScoreColIndexs <- integer(length = pmax - 2)
	for(p in 3:pmax){
		DateColNames[p-2] <- paste("point", p, "_from2ndShot", sep = "")
		ScoreColNames[p-2] <- paste("point", p, "_score", sep = "")
		DateColIndexs[p-2] <- grep(pattern = DateColNames[p-2], colnames(temp))
		ScoreColIndexs[p-2] <- grep(pattern = ScoreColNames[p-2], colnames(temp))
	}
	
	getThalf <- function(C0, Ct, t){
		Thalf <- t/log(C0/Ct, base = 2)
		return(Thalf)
	}
	
	outList <- list()
	for(i in 1:nrow(temp)){
		ID <- IDs[i]
		
		ID.scores <- temp[i, ScoreColIndexs]
		ID.dates <- temp[i, DateColIndexs]
		NA.index <- is.na(ID.scores) == FALSE & is.na(ID.dates) == FALSE
		ID.scores <- ID.scores[NA.index]
		ID.dates <- ID.dates[NA.index]
		
		if(length(ID.scores) >= 2 & length(ID.scores) == length(ID.dates)){
			ID.DF <- data.frame(ID = ID, 
						START = as.integer(ID.dates[1:(length(ID.dates)-1)]), 
						END = as.integer(ID.dates[2:length(ID.dates)]), 
						SPAN = as.integer(NA), MID = as.integer(NA), 
						C0 = as.numeric(ID.scores[1:(length(ID.scores)-1)]), 
						Ct = as.numeric(ID.scores[2:length(ID.scores)]), 
						Thalf = as.numeric(NA))
			ID.DF$SPAN <- ID.DF$END - ID.DF$START
			ID.DF$MID <- ID.DF$START + floor(ID.DF$SPAN/2)
			ID.DF$Thalf <- unlist(Map(f = getThalf, C0 = ID.DF$C0, Ct = ID.DF$Ct, t = ID.DF$SPAN), use.names = FALSE)
			outList[[ID]] <- ID.DF
		}
		if(length(ID.scores) < 2 | length(ID.scores) != length(ID.dates)){
			if(length(ID.scores) != length(ID.dates)){
				stop("check the validity of scores and dates for ", ID, "\n")
			}
			outList[[ID]] <- NA
		}
	}
	if(output == "list") return(outList)
	if(output == "data.frame" | output == "xlsx"){
		outDF <- outList[[1]]
		if(length(outList) > 1){
			for(l in 2:length(outList)){
				if(class(outList[[l]]) == "data.frame"){
					outDF <- rbind(outDF, outList[[l]])
				}
			}
		}
		if(output == "data.frame")	return(outDF)
		if(output == "xlsx")		write.xlsx(outDF, file = OutFileName)
	}
}
