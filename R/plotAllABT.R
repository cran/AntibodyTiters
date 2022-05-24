plotAllABT <- function(objName = "inData", prefix = "",
			dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "weeks", rainbow = FALSE, ylab = "Titer (AU/ml)", 
			savePDF = FALSE, alphaFactor = 10, lwd = 2, lineAttrib = "", addPoints = FALSE, 
			orderOfCategories = "", lowessSmooth = FALSE, geometricMean = FALSE, lineForPre = FALSE, lineFor1st = FALSE, 
			logY = TRUE, PDFwidth = 8, PDFheight = 5, main = NULL, omitPreVac = FALSE, lineColDark = FALSE){
	# orderOfCategories = c("young", "middle", "elderly")
	
	if(is.null(main) == TRUE) main <- objName
				
	if(type != "days" & type != "weeks"){
		cat("discrete mode\n")
		plotAllABT_discrete(objName = objName, prefix = prefix, dayStart = dayStart, dayEnd = dayEnd, 
			type = type, rainbow = rainbow, ylab = ylab, savePDF = savePDF, alphaFactor = alphaFactor, 
			lwd = lwd, lineAttrib = lineAttrib, addPoints = addPoints, 
			orderOfCategories = orderOfCategories, lowessSmooth = lowessSmooth, 
			geometricMean = geometricMean, lineForPre = lineForPre, lineFor1st = lineFor1st, logY = logY, 
			PDFwidth = PDFwidth, PDFheight = PDFheight, main = main, omitPreVac = omitPreVac, 
			lineColDark = lineColDark)
	}
	
	if(type == "days" | type == "weeks"){	
	if(inherits(get(objName), "ABT") != TRUE) stop("The class must be ABT\n")
	if(is.na(dayStart) == FALSE & is.na(dayEnd) == FALSE & dayStart >= dayEnd){
		stop("dayStart must be smaller than dayEnd")
	}
	if(prefix == ""){
		fileName <- paste(objName, "_", type, "_All", ".pdf", sep = "")
	}
	if(prefix != ""){
		fileName <- paste(prefix, "_", objName, "_", type, "_All", 
			".pdf", sep = "")
	}
	if(prefix == "" & lineAttrib != ""){
		fileName <- paste(objName, "_", lineAttrib, "_", type, "_All", ".pdf", sep = "")
	}
	if(prefix != "" & lineAttrib != ""){
		fileName <- paste(prefix, "_", objName, "_", lineAttrib, "_", type, "_All", 
			".pdf", sep = "")
	}
	
	temp <- get(objName)$DATA
	pmax <- get(objName)$pmax
	Attrib <- get(objName)$Attrib
	
	if(lineAttrib != "" & orderOfCategories[1] != ""){
		attributeCategories <- names(Attrib[[lineAttrib]])
		if(length(attributeCategories) != length(orderOfCategories)){
			stop(paste("Contents of orderOfCategories do not match those in the provided object ", objName, ".\n", sep = ""))
		}
		attributeCategories <- attributeCategories[order(attributeCategories)]
		for(a in 1:length(attributeCategories)){
			if(attributeCategories[a] != orderOfCategories[order(orderOfCategories)][a]){
				stop(paste("Contents of orderOfCategories do not match those in the provided object ", objName, ".\n", sep = ""))
			}
		}
	}

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
        stop("type must be weeks, days, M1, M2, M3, M4 or M6.")
    }

	xRange <- longestFromSecond - shortestFromSecond
	posPre <- shortestFromSecond - xRange*2/8
	pos1st <- shortestFromSecond - xRange*1/8
	# xFromSecond <- c(posPre, pos1st, from.secondShot[-(1:2)])		
	
	if(omitPreVac == FALSE)	xlim <- c(posPre, longestFromSecond)
	if(omitPreVac == TRUE)	xlim <- c(pos1st, longestFromSecond)
	
	#ylim <- c(0, max(temp[, grep(pattern = "score", colnames(temp))], na.rm = TRUE))

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
    
	if(savePDF == TRUE)	pdf(file = fileName, width = PDFwidth, height = PDFheight)
    
	if(lineAttrib != "" & inherits(temp2[[lineAttrib]], "numeric") != TRUE){
			categories <- names(Attrib[[lineAttrib]])
			categories <- categories[order(categories)]
			if(orderOfCategories[1] != "")	categories <- orderOfCategories
			COLs <- hcl.colors(n = length(categories), palette = "Dynamic", rev = TRUE, alpha = 0.8)
			COLsDark <- rgb(t(col2rgb(COLs)/1.2), maxColorValue=255)
			pointCOLs <- hcl.colors(n = length(categories), palette = "Dynamic", rev = TRUE, alpha = 0.3)
			lineCOLs <- hcl.colors(n = length(categories), palette = "Dynamic", rev = TRUE, alpha = alpha)
	}

    total.scores <- temp2[, grep(pattern = "_score", x = colnames(temp2))]
    
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
			cat(addToY, " was added to the time points of score=0.\n", sep = "")
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

	if(omitPreVac == FALSE)	axis(1, at = c(posPre, pos1st), labels = c("pre", "1st"), lwd = lwd)
	if(omitPreVac == TRUE)	axis(1, at = c(pos1st), labels = c("1st"), lwd = lwd)
	
	if(type == "weeks") axis(1, at = seq(floor(shortestFromSecond/10)*10, longestFromSecond, 10), lwd = lwd)
	if(type == "days") axis(1, at = seq(floor(shortestFromSecond/50)*50, longestFromSecond, 50), lwd = lwd)
	# axis(2, lwd = lwd)
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
		temp2.scores <- c(temp2.scores[1:2], temp2.scores[-(1:2)][is.na(temp2.scores[-(1:2)]) == FALSE])
		from.secondShot <- c(from.secondShot[1:2], from.secondShot[-(1:2)][is.na(from.secondShot[-(1:2)]) == FALSE])
		temp2.scores <- c(temp2.scores[1:2], temp2.scores[-(1:2)][from.secondShot[-(1:2)] >= shortestFromSecond])
		from.secondShot <- c(from.secondShot[1:2], from.secondShot[-(1:2)][from.secondShot[-(1:2)] >= shortestFromSecond])

		# if(length(temp2.scores) == 3 | length(from.secondShot) == 3){
		if(length(temp2.scores) == 2 | length(from.secondShot) == 2){
			cat(paste("There is no enough data for ", patientID, "\n", sep = ""))
			warning(paste("No plot was drawn for ", patientID, "\n", sep = ""))
		}

		# if(length(temp2.scores) > 3 & length(from.secondShot) > 3){
		if(length(temp2.scores) > 2 & length(from.secondShot) > 2){
		xFromSecond <- c(posPre, pos1st, from.secondShot[-(1:2)])
		
		
		if(logY == TRUE){
			if(length(temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE]) > 0){
				# temp.ymin <- min(temp2.scores[temp2.scores != 0 & is.na(temp2.scores) == FALSE])
				# addToY <- 10^(floor(log(temp.ymin*1000, base = 10))-3)
				temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] <- 
					temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] + addToY
				# cat(addToY, " was added to the time points of score=0.\n", sep = "")
			}
		}

        if(rainbow == TRUE){
                if(omitPreVac == FALSE){
                	lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lty = 1, lwd = lwd, 
                					col = rainbow(1, start = p/patients.max, alpha = alpha))
                }
                if(omitPreVac == TRUE){
                	lines(x = xFromSecond[2:3], y = temp2.scores[2:3], lty = 1, lwd = lwd, 
                					col = rainbow(1, start = p/patients.max, alpha = alpha))
                }
                lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd, 
                					col = rainbow(1, start = p/patients.max, alpha = alpha))
        }
        if(rainbow == FALSE){
        	if(lineAttrib == "" | inherits(temp2[[lineAttrib]], "numeric") == TRUE){
                if(omitPreVac == FALSE){
                	lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lty = 1, lwd = lwd, col = gray(0, alpha = alpha)) 
                }
                if(omitPreVac == TRUE){
                	lines(x = xFromSecond[2:3], y = temp2.scores[2:3], lty = 1, lwd = lwd, col = gray(0, alpha = alpha)) 
                }
                lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd, col = gray(0, alpha = alpha))
            }
        	if(lineAttrib != "" & inherits(temp2[[lineAttrib]], "numeric") != TRUE){
        		c <- which(temp2[[lineAttrib]][p] == categories)
                if(omitPreVac == FALSE){
                	lines(x = xFromSecond[1:3], y = temp2.scores[1:3], lty = 1, lwd = lwd, col = lineCOLs[c]) 
                }
                if(omitPreVac == TRUE){
                	lines(x = xFromSecond[2:3], y = temp2.scores[2:3], lty = 1, lwd = lwd, col = lineCOLs[c]) 
                }
                lines(x = xFromSecond[-(1:2)], y = temp2.scores[-(1:2)], lwd = lwd, col = lineCOLs[c])
            }
		}
		}
	}

	
	if(lineAttrib != ""){
		if(length(grep(pattern = lineAttrib, x = names(Attrib))) == 0){
			stop("lineAttrib must be found in the Attrib of the given ABT object")
		}
		if(inherits(temp2[[lineAttrib]], "numeric") == TRUE){
			stop("lineAttrib cannot be applied for numeric values")
		}
		if(inherits(temp2[[lineAttrib]], "numeric") != TRUE){
			for(c in 1:length(categories)){
				
				if(is.na(dayStart) == TRUE)	shortestFromSecond <- get(objName)$shortestFromSecond
				if(is.na(dayEnd) == TRUE)	longestFromSecond <- get(objName)$longestFromSecond
				if(is.na(dayStart) == FALSE)	shortestFromSecond <- dayStart
				if(is.na(dayEnd) == FALSE)	longestFromSecond <- dayEnd

				temp3 <- subset(temp2, temp2[[lineAttrib]] == categories[c])
				
				temp3.scores <- temp3[, grep(pattern = "score", colnames(temp3))]
				temp3.scores <- temp3.scores[, -c(1:2)]
				temp3.dates <- temp3[, grep(pattern = "_from2ndShot", colnames(temp3))]
				temp3.dates <- temp3.dates[, grep(pattern = "point", colnames(temp3.dates))]
				# temp3.dates <- temp3.dates[, -grep(pattern = "point3", colnames(temp3.dates))]
				
				RAW <- list()
				# RAW[shortestFromSecond:longestFromSecond] <- as.numeric(NA)
				RAW[(shortestFromSecond:longestFromSecond)+1] <- as.numeric(NA)
				for(dcol in 1:ncol(temp3.dates)){
					for(drow in 1:nrow(temp3.dates)){
						# x.index <- temp3.dates[drow, dcol]	# min=0
						x.index <- temp3.dates[drow, dcol] +1
						# if(is.na(x.index) == FALSE & x.index == 9) cat("drow:", drow, ", dcol:", dcol, "\n")
						if(is.na(dayStart) == FALSE & is.na(dayEnd) == FALSE){
							if(is.na(x.index) == FALSE & x.index >= dayStart & x.index <= dayEnd){
								score <- temp3.scores[drow, dcol]
								RAW[[x.index]] <- c(RAW[[x.index]], score)
							}
						}
						if(is.na(dayStart) == FALSE & is.na(dayEnd) == TRUE){
							if(is.na(x.index) == FALSE & x.index >= dayStart){
								score <- temp3.scores[drow, dcol]
								RAW[[x.index]] <- c(RAW[[x.index]], score)
							}
						}
						if(is.na(dayStart) == TRUE & is.na(dayEnd) == FALSE){
							if(is.na(x.index) == FALSE & x.index <= dayEnd){
								score <- temp3.scores[drow, dcol]
								RAW[[x.index]] <- c(RAW[[x.index]], score)
							}
						}
						if(is.na(dayStart) == TRUE & is.na(dayEnd) == TRUE){
							if(is.na(x.index) == FALSE){
								score <- temp3.scores[drow, dcol]
								RAW[[x.index]] <- c(RAW[[x.index]], score)
							}
						}
					}
				}
				
				RAW2 <- data.frame(x = shortestFromSecond:longestFromSecond, score = NA)
				for(i in shortestFromSecond:longestFromSecond){
					# scores <- RAW[[i]][is.na(RAW[[i]]) == FALSE]
					scores <- RAW[[i+1]][is.na(RAW[[i+1]]) == FALSE]
					if(length(scores) > 0)	RAW2$score[RAW2$x == i] <- mean(scores)
				}
				
				RAW3 <- subset(RAW2, is.na(RAW2$score) == FALSE & RAW2$score != 0)
				if(logY == FALSE){
					RAW3$l <- lowess(RAW3)$y
				}
				if(logY == TRUE){
					RAW3$scoreLog <- log(RAW3$score, base = 10)
					RAW3$lLog <- lowess(x = RAW3$x, y = RAW3$scoreLog)$y
					RAW3$l <- 10^RAW3$lLog
				}
				
				if(type == "weeks"){
					RAW3$x <- RAW3$x*1/7
					shortestFromSecond <- shortestFromSecond*1/7
					longestFromSecond <- longestFromSecond*1/7
				}
				if(addPoints == TRUE){
					if(omitPreVac == FALSE){
                		points(x = RAW3$x, y = RAW3$score, pch = 20, cex = 0.8, col = pointCOLs[c])
                	}
					if(omitPreVac == TRUE){
                		points(x = RAW3$x[2:nrow(RAW3)], y = RAW3$score[2:nrow(RAW3)], pch = 20, cex = 0.8, col = pointCOLs[c])
                	}
				}
				
				if(lowessSmooth == TRUE){
					if(lineColDark == FALSE){
						if(omitPreVac == FALSE){
                			lines(x = RAW3$x, y = RAW3$l, col = COLs[c], lwd = lwd*2)
                		}
						if(omitPreVac == TRUE){
                			lines(x = RAW3$x[2:nrow(RAW3)], y = RAW3$l[2:nrow(RAW3)], col = COLs[c], lwd = lwd*2)
                		}
                	}
					if(lineColDark == TRUE){
						if(omitPreVac == FALSE){
                			lines(x = RAW3$x, y = RAW3$l, col = COLsDark[c], lwd = lwd*2)
                		}
						if(omitPreVac == TRUE){
                			lines(x = RAW3$x[2:nrow(RAW3)], y = RAW3$l[2:nrow(RAW3)], col = COLsDark[c], lwd = lwd*2)
                		}
                	}
				}
								
				legendX <- longestFromSecond - (longestFromSecond - shortestFromSecond)/3		# 3 or 4
				if(logY == FALSE)	legendY <- ylim[2] - (ylim[2] - ylim[1])/10*c
				if(logY == TRUE)	legendY <- 10^(log(ylim[2], base = 10) - 
								((log(ylim[2], base = 10) - log(ylim[1], base = 10))/15*(c)))	# c+1 or c?
				if(lineColDark == FALSE){
						legend(x = legendX, y = legendY, col = COLs[c], lwd = lwd*2, 
							legend = categories[c], bty = "n")
				}
				if(lineColDark == TRUE){
						legend(x = legendX, y = legendY, col = COLsDark[c], lwd = lwd*2, 
							legend = categories[c], bty = "n")
				}
				if(c == 1){
					legendX <- longestFromSecond - (longestFromSecond - shortestFromSecond)/3.5		# 4.5
					# legendY <- ylim[2] - (ylim[2] - ylim[1])/10*c * 1.4
					if(logY == FALSE)	legendY <- ylim[2] - (ylim[2] - ylim[1])/10*c * 1.4
					if(logY == TRUE)	legendY <- 10^(log(ylim[2], base = 10) - 
								((log(ylim[2], base = 10) - log(ylim[1], base = 10))/20*(c+1)))	# c+1
					text(x = legendX, y = legendY, pos = 3, labels = lineAttrib)
				}

			}
		}
	}
	
	if(savePDF == TRUE)	dev.off()
	}
}
