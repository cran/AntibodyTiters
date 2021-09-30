plotEachABT <- function(patientID = "patient A", objName = "inData", prefix = "", 
			dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks",
            ylab = "Titer (AU)", savePDF = FALSE, addPoints = FALSE, lwd = 2){
	if(class(get(objName)) != "ABT") stop("The class must be ABT\n")
	if(is.na(dayStart) == FALSE & is.na(dayEnd) == FALSE & dayStart >= dayEnd){
		stop("dayStart must be smaller than dayEnd")
	}
	if(prefix == ""){
		fileName <- paste(objName, "_", 
			sub(pattern = " ", replacement = "_", patientID), 
			".pdf", sep = "")
	}
	if(prefix != ""){
		fileName <- paste(prefix, "_", objName, "_", 
			sub(pattern = " ", replacement = "_", patientID), 
			".pdf", sep = "")
	}
	
	temp <- get(objName)$DATA
	pmax <- get(objName)$pmax

	if(is.na(dayStart) == TRUE)	shortestFromSecond <- get(objName)$shortestFromSecond
	if(is.na(dayEnd) == TRUE)	longestFromSecond <- get(objName)$longestFromSecond
	if(is.na(dayStart) == FALSE)	shortestFromSecond <- dayStart
	if(is.na(dayEnd) == FALSE)	longestFromSecond <- dayEnd

	if(type == "days"){
		xlab <- "Days after second shot"
	}
	if(type == "weeks"){
		xlab <- "Weeks after second shot"
		shortestFromSecond <- shortestFromSecond*1/7
		longestFromSecond <- longestFromSecond*1/7
	}
	if(type != "weeks" & type != "days") {
		stop("type must be weeks or days.")
	}
    
	temp2 <- subset(temp, temp$ID == patientID)

	temp2.scores <- temp2[1, grep(pattern = "score", colnames(temp2))]
	temp2.dates <- temp2[1, grep(pattern = "yyyymmdd", colnames(temp2))]

	secondShot <- temp2.dates[["2nd_shot_yyyymmdd"]][1]

	temp2.dates <- temp2.dates[, colnames(temp2.dates) != "1st_shot_yyyymmdd"]
	temp2.dates <- temp2.dates[, colnames(temp2.dates) != "2nd_shot_yyyymmdd"]

	from.secondShot <- as.integer(temp2.dates) - as.integer(secondShot)

	if(type == "weeks"){
		from.secondShot <- from.secondShot*1/7
	}

	temp2.scores <- as.integer(temp2.scores)
	temp2.scores <- c(temp2.scores[1:3], temp2.scores[-(1:3)][is.na(temp2.scores[-(1:3)]) == FALSE])
	from.secondShot <- c(from.secondShot[1:3], from.secondShot[-(1:3)][is.na(from.secondShot[-(1:3)]) == FALSE])
	temp2.scores <- c(temp2.scores[1:3], temp2.scores[-(1:3)][from.secondShot[-(1:3)] >= shortestFromSecond])
	from.secondShot <- c(from.secondShot[1:3], from.secondShot[-(1:3)][from.secondShot[-(1:3)] >= shortestFromSecond])

	if(length(temp2.scores) == 3 | length(from.secondShot) == 3){
		warning(paste("There is no enough data for ", patientID, "\n", sep = ""))
		return(paste("No plot was drawn for ", patientID, sep = ""))
	}

	xFromSecond <- c(shortestFromSecond*1/4, shortestFromSecond*2/4, 
		shortestFromSecond*3/4, from.secondShot[-(1:3)])

	if(savePDF == TRUE)	pdf(file = fileName, width = 8, height = 4)
	
	xlim <- c(shortestFromSecond*1/4, longestFromSecond)
	plot(x = xFromSecond, y = temp2.scores, xlim = xlim, xaxt = "n", yaxt = "n", type = "n", 
		xlab = xlab, ylab = ylab,
		main = paste(objName, " ", patientID, sep = ""))
	axis(1, at = xFromSecond[1:3], labels = c("pre", "1st", "2nd"), lwd = lwd)
	if(type == "weeks") axis(1, at = seq(ceiling(shortestFromSecond/10)*10, longestFromSecond, 10), lwd = lwd)
	if(type == "days") axis(1, at = seq(ceiling(shortestFromSecond/50)*50, longestFromSecond, 50), lwd = lwd)
	axis(2, lwd = lwd)
	box(lwd = lwd)
	
	if(addPoints == TRUE){
		points(x = xFromSecond[1:3], y = temp2.scores[1:3], pch = 20)
	}
	lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lwd = lwd)
	lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd)
	if(addPoints == TRUE){
		points(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], pch = 1)
	}

	if(savePDF == TRUE)	dev.off()	

}
