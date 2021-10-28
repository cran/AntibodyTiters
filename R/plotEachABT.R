plotEachABT <- function(patientID = "patient A", objName = "inData", prefix = "", 
			dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks",
            ylab = "Titer (AU/ml)", savePDF = FALSE, addPoints = FALSE, lwd = 2, 
            logY = TRUE, PDFwidth = 8, PDFheight = 5, main = NULL){
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
	
	if(is.null(main) == TRUE){
		main <- paste(patientID, "@", objName, sep = "")
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
	if(nrow(temp2) == 0){
		stop("There is no ", patientID, " in ", objName, "\n", sep = "")
	}

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
	temp2.scores <- c(temp2.scores[1:2], temp2.scores[-(1:2)][is.na(temp2.scores[-(1:2)]) == FALSE])
	from.secondShot <- c(from.secondShot[1:2], from.secondShot[-(1:2)][is.na(from.secondShot[-(1:2)]) == FALSE])
	temp2.scores <- c(temp2.scores[1:2], temp2.scores[-(1:2)][from.secondShot[-(1:2)] >= shortestFromSecond])
	from.secondShot <- c(from.secondShot[1:2], from.secondShot[-(1:2)][from.secondShot[-(1:2)] >= shortestFromSecond])

	# if(length(temp2.scores) == 3 | length(from.secondShot) == 3){
	if(length(temp2.scores) == 2 | length(from.secondShot) == 2){
		warning(paste("There is no enough data for ", patientID, "\n", sep = ""))
		return(paste("No plot was drawn for ", patientID, sep = ""))
	}

	# xFromSecond <- c(shortestFromSecond*1/3, shortestFromSecond*2/3, 
	# 	from.secondShot[-(1:2)])
	xRange <- longestFromSecond - shortestFromSecond
	posPre <- shortestFromSecond - xRange*2/8
	pos1st <- shortestFromSecond - xRange*1/8
	xFromSecond <- c(posPre, pos1st, from.secondShot[-(1:2)])		

	xlim <- c(posPre, longestFromSecond)
	
	if(savePDF == TRUE)	pdf(file = fileName, width = PDFwidth, height = PDFheight)
		
    total.scores <- temp[, grep(pattern = "_score", x = colnames(temp))]
    
	if(logY == FALSE){
		ymin <- min(total.scores, na.rm = TRUE)
		ymax <- max(total.scores, na.rm = TRUE)
		ymax <- ymax * 1.1
		ylim <- c(ymin, ymax)
		plot(x = 1:2, y = numeric(length = 2), xlim = xlim, ylim = ylim, main = main, 
			xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab)
		FROM <- floor(ymin/10000)
		# BY <- floor((ymax - ymin)/5/100)*10^floor(log(ymax, base = 10))
		TO <- ceiling(ymax/10^(floor(log(ymax, base = 10))))*10^(floor(log(ymax, base = 10)))
		# BY <- (TO - FROM)/5
		BY <- ceiling((TO - FROM)/8/(10^floor(log((TO - FROM)/8, base = 10)))) * 10^floor(log((TO - FROM)/8, base = 10))
		axis(2, at = seq(from = FROM, to = TO, by = BY), lwd = lwd)
	}

	if(logY == TRUE){
		if(length(total.scores[total.scores == 0]) > 0){
			temp.ymin <- min(total.scores[total.scores != 0 & is.na(total.scores) == FALSE])
			addToY <- 10^(floor(log(temp.ymin*1000, base = 10))-3)
			for(n in 1:ncol(total.scores)){
				total.scores[total.scores[,n] == 0 & is.na(total.scores[,n]) == FALSE, n] <- 
					total.scores[total.scores[,n] == 0 & is.na(total.scores[,n]) == FALSE, n] + addToY
			}
			# cat(addToY, " was added to the time points of score=0.\n", sep = "")
		}
		
		ymin <- min(total.scores[total.scores != 0 & is.na(total.scores) == FALSE])*0.9
		ymax <- max(total.scores, na.rm = TRUE)
		ymax <- ymax * 11	# 5
		ylim <- c(ymin, ymax)
		plot(x = 1:2, y = c(1, 1), xlim = xlim, ylim = ylim, main = main, 
			xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab, log = "y")
		FROM <- floor(log(ymin, base = 10))
		TO <- floor(log(ymax, base = 10))+1
		AT <- integer(length = (TO - FROM + 1) * 10 - (TO - FROM))
		for(i in 1:length(FROM:TO)){
			p <- seq(FROM, TO, 1)[i]
			if(p == FROM){
				START <- ((i-1)*10+1)
				END <- ((i-1)*10+10)
				AT[START:END] <- seq(from = (10^p)/10, to = (10^(p+1))/10, by = (10^p)/10)
			}
			if(p != FROM){
				START <- END
				END <- END + 9
				AT[START:END] <- seq(from = (10^p)/10, to = (10^(p+1))/10, by = (10^p)/10)
			}
		}
		axis(2, at = AT, labels = FALSE, lwd = lwd/2)
		AT <- integer(length = (TO - FROM + 1))
		for(i in 1:length(FROM:TO)){
			p <- seq(FROM, TO, 1)[i]
			AT[i] <- (10^(p+1))/10
		}
		axis(2, at = AT, labels = formatC(AT, format = "g"), lwd = lwd)
		if(length(AT) < 5){
			AT <- integer(length = (TO - FROM + 1))
			for(i in 1:length(FROM:TO)){
				p <- seq(FROM, TO, 1)[i]
				AT[i] <- (10^(p+1))/10*5
			}
			axis(2, at = AT, labels = formatC(AT, format = "g"), lwd = lwd)
		}
	}

	axis(1, at = xFromSecond[1:2], labels = c("pre", "1st"), lwd = lwd)
	if(type == "weeks") axis(1, at = seq(floor(shortestFromSecond/10)*10, longestFromSecond, 10), lwd = lwd)
	if(type == "days") axis(1, at = seq(floor(shortestFromSecond/50)*50, longestFromSecond, 50), lwd = lwd)
	# axis(2, lwd = lwd)
	box(lwd = lwd)
	
	if(logY == TRUE){
		if(length(temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE]) > 0){
			# temp.ymin <- min(temp2.scores[temp2.scores != 0 & is.na(temp2.scores) == FALSE])
			# addToY <- 10^(floor(log(temp.ymin*1000, base = 10))-3)
			temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] <- 
				temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] + addToY
			cat(addToY, " was added to the time points of score=0.\n", sep = "")
		}
	}

	if(addPoints == TRUE){
		points(x = xFromSecond[1:2], y = temp2.scores[1:2], pch = 20)
	}
	lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lwd = lwd)
	lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd)
	if(addPoints == TRUE){
		points(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], pch = 1)
	}

	if(savePDF == TRUE)	dev.off()	

}
