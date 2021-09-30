plotAllABT <- function(objName = "inData", prefix = "",
			dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks", rainbow = FALSE, ylab = "Titer (AU)", 
			savePDF = FALSE, alphaFactor = 10, lwd = 2){
	if(class(get(objName)) != "ABT") stop("The class must be ABT\n")
	if(is.na(dayStart) == FALSE & is.na(dayEnd) == FALSE & dayStart >= dayEnd){
		stop("dayStart must be smaller than dayEnd")
	}
	if(prefix == ""){
		fileName <- paste(objName, "_All", ".pdf", sep = "")
	}
	if(prefix != ""){
		fileName <- paste(prefix, "_", objName, "_All", 
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

	xlim <- c(shortestFromSecond*1/4, longestFromSecond)
	ylim <- c(0, max(temp[, grep(pattern = "score", colnames(temp))], na.rm = TRUE))

	temp2 <- temp

    patients.pop <- nrow(temp2)
    # if(1 <= patients.pop && patients.pop <= 30) alpha <- 0.5
    # if(30 < patients.pop && patients.pop <= 70) alpha <- 0.3
    # if(70 < patients.pop) alpha <- 0.1
    alpha <- round(1/patients.pop*alphaFactor, digits = 2)
    if(alpha > 1)   alpha <- 1
    if(alphaFactor <= 0 | alphaFactor > 100){
        stop("alphaFactor should be in a range from 1 to 100")
    }
       
	if(savePDF == TRUE)	pdf(file = fileName, width = 8, height = 4)
	plot(x = 1:2, y = numeric(length = 2), xlim = xlim, ylim = ylim, main = objName, 
		xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab)
	axis(1, at = c(shortestFromSecond*1/4, shortestFromSecond*2/4, 
			shortestFromSecond*3/4), labels = c("pre", "1st", "2nd"), lwd = lwd)
	if(type == "weeks") axis(1, at = seq(ceiling(shortestFromSecond/10)*10, longestFromSecond, 10), lwd = lwd)
	if(type == "days") axis(1, at = seq(ceiling(shortestFromSecond/50)*50, longestFromSecond, 50), lwd = lwd)
	axis(2, lwd = lwd)
	box(lwd = lwd)

        patients.max <- nrow(temp2)+1
       
	for(p in 1:nrow(temp2)){
		patientID <- temp2$ID[p]
		temp2.scores <- temp2[p, grep(pattern = "score", colnames(temp2))]
		temp2.dates <- temp2[p, grep(pattern = "yyyymmdd", colnames(temp2))]

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
			cat(paste("There is no enough data for ", patientID, "\n", sep = ""))
			warning(paste("No plot was drawn for ", patientID, "\n", sep = ""))
		}

		if(length(temp2.scores) > 3 & length(from.secondShot) > 3){
		xFromSecond <- c(shortestFromSecond*1/4, shortestFromSecond*2/4, 
			shortestFromSecond*3/4, from.secondShot[-(1:3)])

        if(rainbow == TRUE){
                lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lty = 1, lwd = lwd, col = rainbow(1, start = p/patients.max, alpha = alpha)) 
                lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd, col = rainbow(1, start = p/patients.max, alpha = alpha))
                }
                if(rainbow == FALSE){
                lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lty = 1, lwd = lwd, col = gray(0, alpha = alpha)) 
                lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd, col = gray(0, alpha = alpha))
		}
		}
	}
	if(savePDF == TRUE)	dev.off()	
}
