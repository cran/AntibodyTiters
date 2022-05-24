plotAllABT_discrete <- function(objName = "inData", prefix = "",
			dayStart = as.integer(NA), dayEnd = as.integer(NA), type = "M1", rainbow = FALSE, ylab = "Titer (AU)", 
			savePDF = FALSE, alphaFactor = 10, lwd = 2, lineAttrib = "", addPoints = FALSE, 
			orderOfCategories = "", lowessSmooth = FALSE, geometricMean = FALSE, lineForPre = FALSE, lineFor1st = FALSE, 
			logY = TRUE, PDFwidth = 8, PDFheight = 5, main = NULL, omitPreVac = FALSE, lineColDark = FALSE){
	# orderOfCategories = c("young", "middle", "elderly")
	
	if(is.null(main) == TRUE) main <- objName
								
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

	xlab <- "Months after second shot"
        
    colNames <- character(length = 0)
    x.labels <- character(length = 0)
	post2ndDays <- as.integer(temp[["point3_yyyymmdd"]] - temp[["2nd_shot_yyyymmdd"]])
	
    c <- 1
    if(type == "M1"){
		firstMonth <- min(ceiling(shortestFromSecond/30), ceiling(post2ndDays/30), na.rm = TRUE)
		if(firstMonth == 0) firstMonth <- 1
		lastMonth <- ceiling(longestFromSecond/30)
		for(m in firstMonth:lastMonth){
			colNames[c] <-paste("M1_", m, "_from2ndShot", sep = "")
			x.labels[c] <- m
			c <- c + 1
		}
    }
    if(type == "M2"){
		firstM2group <- min(ceiling(shortestFromSecond/60), ceiling(post2ndDays/60), na.rm = TRUE)
		if(firstM2group == 0) firstM2group <- 1
		lastM2group <- ceiling(longestFromSecond/60)
		for(m in firstM2group: lastM2group){
			M2start <- (m-1)*2 + 1
			M2end <- (m-1)*2 + 2
			colNames[c] <- paste("M2_", M2start, "-", M2end, "_from2ndShot", sep = "")
			x.labels[c] <- paste(M2start, "-", M2end, sep = "")
			c <- c + 1
		}
    }
    if(type == "M3"){
		firstM3group <- min(ceiling(shortestFromSecond/90), ceiling(post2ndDays/90), na.rm = TRUE)
		if(firstM3group == 0) firstM3group <- 1
		lastM3group <- ceiling(longestFromSecond/90)
		for(m in firstM3group: lastM3group){
			M3start <- (m-1)*3 + 1
			M3end <- (m-1)*3 + 3
			colNames[c] <- paste("M3_", M3start, "-", M3end, "_from2ndShot", sep = "")
			x.labels[c] <- paste(M3start, "-", M3end, sep = "")
			c <- c + 1
		}
    }
    if(type == "M4"){
		firstM4group <- min(ceiling(shortestFromSecond/120), ceiling(post2ndDays/120), na.rm = TRUE)
		if(firstM4group == 0) firstM4group <- 1
		lastM4group <- ceiling(longestFromSecond/120)
		for(m in firstM4group: lastM4group){
			M4start <- (m-1)*4 + 1
			M4end <- (m-1)*4 + 4
			colNames[c] <- paste("M4_", M4start, "-", M4end, "_from2ndShot", sep = "")
			x.labels[c] <- paste(M4start, "-", M4end, sep = "")
			c <- c + 1
		}
    }
    if(type == "M6"){
		firstM6group <- min(ceiling(shortestFromSecond/180), ceiling(post2ndDays/180), na.rm = TRUE)
		if(firstM6group == 0) firstM6group <- 1
		lastM6group <- ceiling(longestFromSecond/180)
		for(m in firstM6group: lastM6group){
			M6start <- (m-1)*6 + 1
			M6end <- (m-1)*6 + 6
			colNames[c] <- paste("M6_", M6start, "-", M6end, "_from2ndShot", sep = "")
			x.labels[c] <- paste(M6start, "-", M6end, sep = "")
			c <- c + 1
		}
    }
    
    if(type != "M1" & type != "M2" & type != "M3" & type != "M4" & type != "M6") {
        stop("type must be weeks, days, M1, M2, M3, M4 or M6.")
    }

	# x <- c(1:3, 4:(3+length(colNames)))
	x <- c(1:2, 3:(2+length(colNames)))
	# if(omitPreVac == FALSE)	x <- c(1:2, 3:(2+length(colNames)))
	# if(omitPreVac == TRUE)	x <- c(-1, 2, 3:(2+length(colNames)))
	# ylim <- c(0, max(temp[, grep(pattern = "score", colnames(temp))], na.rm = TRUE))

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
		if(omitPreVac == FALSE){
			plot(x = x, y = numeric(length = length(x)), ylim = ylim, main = main, 
				xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab)
		}
		if(omitPreVac == TRUE){
			plot(x = x[2:length(x)], y = numeric(length = length(x)-1), ylim = ylim, main = main, 
				xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab)
		}
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
		xtemp <- numeric(length = length(x))
		xtemp[1:length(x)] <- 1
		if(omitPreVac == FALSE){
			plot(x = x, y = xtemp, ylim = ylim, main = main, 
				xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab, log = "y")
		}
		if(omitPreVac == TRUE){
			plot(x = x[2:length(x)], y = xtemp[2:length(x)], ylim = ylim, main = main, 
				xaxt = "n", yaxt = "n", type = "n", xlab = xlab, ylab = ylab, log = "y")
		}
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

	if(omitPreVac == FALSE){
		axis(1, at = 1:2, labels = c("pre", "1st"), lwd = lwd)
		axis(1, at = 3:length(x), labels = x.labels, lwd = lwd)
	}
	if(omitPreVac == TRUE){
		axis(1, at = c(-1, 2), labels = c("pre", "1st"), lwd = lwd)
		axis(1, at = 3:length(x), labels = x.labels, lwd = lwd)
	}
	
	box(lwd = lwd)

        patients.max <- nrow(temp2)+1
       
	for(p in 1:nrow(temp2)){
		patientID <- temp2$ID[p]
		# temp2.scores <- as.numeric(temp2[p, grep(pattern = "score", colnames(temp2))][1:3])
		temp2.scores <- as.numeric(temp2[p, grep(pattern = "score", colnames(temp2))][1:2])
		for(c in 1:length(colNames)){
			colName <- colNames[c]
			# temp2.scores[3 + c] <- temp2[[colName]][p]
			temp2.scores[2 + c] <- temp2[[colName]][p]
		}
		
		
		if(logY == TRUE){
			if(length(temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE]) > 0){
				# temp.ymin <- min(temp2.scores[temp2.scores != 0 & is.na(temp2.scores) == FALSE])
				# addToY <- 10^(floor(log(temp.ymin*1000, base = 10))-3)
				temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] <- 
					temp2.scores[temp2.scores == 0 & is.na(temp2.scores) == FALSE] + addToY
				# cat(addToY, " was added to the time points of score=0.\n", sep = "")
			}
		}
		
		x.patient <- x[is.na(temp2.scores) == FALSE]
		temp2.scores.patient <- temp2.scores[is.na(temp2.scores) == FALSE]
		
        # if(omitPreVac == TRUE){
		# 	x.patient <- x.patient[x.patient != 1]
		# 	temp2.scores.patient <- temp2.scores.patient[x.patient != 1]
		# }

        if(rainbow == TRUE){
                lines(x = x.patient, 
                		y = temp2.scores.patient, lty = 1, lwd = lwd, 
                		col = rainbow(1, start = p/patients.max, alpha = alpha)) 
        }
        if(rainbow == FALSE){
        	if(lineAttrib == "" | inherits(temp2[[lineAttrib]], "numeric") == TRUE){
               lines(x = x.patient, 
                		y = temp2.scores.patient, lty = 1, lwd = lwd, col = gray(0, alpha = alpha)) 
            }
        	if(lineAttrib != "" & inherits(temp2[[lineAttrib]], "numeric") != TRUE){
        		c <- which(temp2[[lineAttrib]][p] == categories)
			    lines(x = x.patient, 
                		y = temp2.scores.patient, lty = 1, lwd = lwd, col = lineCOLs[c])
            }
		}
	}
	
	if(lineAttrib != ""){
		# require(DescTools)
		if(length(grep(pattern = lineAttrib, x = names(Attrib))) == 0){
			stop("lineAttrib must be found in the Attrib of the given ABT object")
		}
		if(inherits(temp2[[lineAttrib]], "numeric") == TRUE){
			stop("lineAttrib cannot be applied for numeric values")
		}
		if(inherits(temp2[[lineAttrib]], "numeric") != TRUE){
			for(c in 1:length(categories)){
				
				temp3 <- subset(temp2, temp2[[lineAttrib]] == categories[c])
				
				if(type == "M1")	temp3.scores <- temp3[, grep(pattern = "M1_", colnames(temp3))]
				if(type == "M2")	temp3.scores <- temp3[, grep(pattern = "M2_", colnames(temp3))]
				if(type == "M4")	temp3.scores <- temp3[, grep(pattern = "M4_", colnames(temp3))]
				if(type == "M3")	temp3.scores <- temp3[, grep(pattern = "M3_", colnames(temp3))]
				if(type == "M6")	temp3.scores <- temp3[, grep(pattern = "M6_", colnames(temp3))]
				
				RAW <- list()
				if(type == "M1"){
					firstMonth <- as.integer(strsplit(colNames[1], split = "_")[[1]][2])
					lastMonth <- as.integer(strsplit(colNames[length(colNames)], split = "_")[[1]][2])
				}
				if(type != "M1"){
					tempM <- strsplit(colNames[1], split = "_")[[1]][2]
					firstMonth <- as.integer(strsplit(tempM, split = "-")[[1]][1])
					tempM <- strsplit(colNames[length(colNames)], split = "_")[[1]][2]
					lastMonth <- as.integer(strsplit(tempM, split = "-")[[1]][1])
				}
				RAW[["Pre"]] <- temp3$pre_vaccination_score
				RAW[["Post1st"]] <- temp3$post_1st_shot_score
				RAW[(firstMonth:lastMonth)+2] <- as.numeric(NA)
				names(RAW)[(firstMonth:lastMonth)+2] <- firstMonth:lastMonth
				
								
				for(m in 1:length(colNames)){
					colName <- colNames[m]
					if(type == "M1"){
						Month <- as.character(strsplit(colName, split = "_")[[1]][2])
					}
					if(type != "M1"){
						tempM <- strsplit(colName, split = "_")[[1]][2]
						Month <- as.character(strsplit(tempM, split = "-")[[1]][1])
					}
					RAW[[Month]] <- temp3.scores[[colName]]
				}
				
				RAW <- RAW[is.na(RAW) == FALSE]
				
				if(omitPreVac == TRUE){
					RAW[["Pre"]] <- NA
				}
				
				if(addPoints == TRUE){
					for(mx in 1:length(RAW)){
						for(y in 1:length(RAW[[mx]])){
							xShiftFactor <- 0.05 * length(x) /12
							# shifted.x <- mx + 3 - ((length(categories) * xShiftFactor)/2) + (c-1) * xShiftFactor
							shifted.x <- mx - ((length(categories) * xShiftFactor)/2) + (c-1) * xShiftFactor
							points(x = shifted.x, y = RAW[[mx]][y], pch = 20, cex = 0.8, col = pointCOLs[c])
						}
					}
				}


				if(lowessSmooth == TRUE){
					RAW2 <- data.frame(x = names(RAW))
					RAW2$score <- NA
					
					for(m in 1:length(RAW)){
						rawValues <- RAW[[m]][is.na(RAW[[m]]) == FALSE & RAW[[m]] >= 0]
						if(length(rawValues) > 0){
							RAW2$score[m] <- mean(rawValues)
						}
					}	
					
					RAW3 <- subset(RAW2, is.na(RAW2$score) == FALSE)
					# RAW3 <- RAW2
					# RAW3$l <- lowess(RAW3)$y
					if(logY == FALSE){
						RAW3$l <- lowess(RAW3)$y
					}
					if(logY == TRUE){
						RAW3$scoreLog <- log(RAW3$score, base = 10)
						RAW3$lLog <- as.numeric(NA)
						RAW3$lLog[-(1:2)] <- lowess(x = RAW3$x[-(1:2)], y = RAW3$scoreLog[-(1:2)])$y
						RAW3$l <- 10^RAW3$lLog
					}
							
					# lowess.x <- x
					lowess.x <- x[is.na(RAW2$score) == FALSE]
					names(lowess.x) <- RAW3$x
					if(lineForPre == FALSE){
						RAW3 <- RAW3[RAW3$x != "Pre",]
						lowess.x <- lowess.x[names(lowess.x) != "Pre"]
					}
					if(lineFor1st == FALSE){
						RAW3 <- RAW3[RAW3$x != "Post1st",]
						lowess.x <- lowess.x[names(lowess.x) != "Post1st"]
					}
					lowess.index <- is.na(RAW3$l) == FALSE
					if(lineColDark == FALSE){
						lines(x = lowess.x[lowess.index], y = RAW3$l[lowess.index], col = COLs[c], lwd = lwd*2)
					}
					if(lineColDark == TRUE){
						lines(x = lowess.x[lowess.index], y = RAW3$l[lowess.index], col = COLsDark[c], lwd = lwd*2)
					}
				} # if(lowessSmooth == TRUE)


				if(geometricMean == TRUE){
					months <- seq(firstMonth, lastMonth, as.integer(strsplit(type, "M")[[1]][2]))
					GMDF <- data.frame(x = c("Pre", "Post1st", as.character(months)), mean = as.numeric(NA), 
							lwr.ci = as.numeric(NA), upr.ci = as.numeric(NA))
					tooLow <- 1
					for(m in 1:length(RAW)){
						rawValues <- RAW[[m]][is.na(RAW[[m]]) == FALSE & RAW[[m]] > 0]
						# if(length(rawValues) > 1){
						if(length(rawValues) > 2){
							GmeanResult <- Gmean(rawValues, conf.level = 0.95, na.rm = TRUE)
							GM <- GmeanResult[1]
							if(is.na(GM) == FALSE)	GMDF$mean[m] <- GM
							LOWCI <- GmeanResult[2]
							if(is.na(LOWCI) == FALSE)	GMDF$lwr.ci[m] <- LOWCI
							UPRCI <- GmeanResult[3]
							if(is.na(UPRCI) == FALSE)	GMDF$upr.ci[m] <- UPRCI
						}
						if(length(rawValues) <= 2){
							if(m <= 2)	DataPoint <- GMDF$x[m]
							if(m > 2)	DataPoint <- paste("M", GMDF$x[m], sep = "")
							if(tooLow > 1)	cat(paste(", ", DataPoint, sep = ""))
							if(tooLow == 1){
								cat(paste("Number of samples is too low (<3) for geometric mean calculation", 
											" of the category \"", categories[c], "\"\n", sep = ""))
								cat(paste("    at ", DataPoint, sep = ""))
								tooLow <- tooLow + 1
							}
						}
					}
					if(tooLow > 1)	cat("\n")
	
					GMs <- numeric(length = length(x))
					GMs[1:length(GMs)] <- NA
					LOWCIs <- numeric(length = length(x))
					LOWCIs[1:length(LOWCIs)] <- NA
					UPRCIs <- numeric(length = length(x))
					UPRCIs[1:length(UPRCIs)] <- NA
					for(m in 1:length(months)){
						GMs[m] <- GMDF$mean[m]
						LOWCIs[m] <- GMDF$lwr.ci[m]
						UPRCIs[m] <- GMDF$upr.ci[m]
					}
					
					GMDF$x.axis <- x
					GMDF <- GMDF[is.na(GMDF$mean) == FALSE, ]
					if(lineForPre == FALSE)	GMDF <- GMDF[GMDF$x != "Pre",]
					if(lineFor1st == FALSE)	GMDF <- GMDF[GMDF$x != "Post1st",]
					
					for(l in 1:nrow(GMDF)){
						xShiftFactor <- 0.05 * length(x) /12
						shifted.x <- GMDF$x.axis[l] - ((length(categories) * xShiftFactor)/2) + (c-1) * xShiftFactor
						if(lineColDark == FALSE){
							lines(x = c(shifted.x, shifted.x), 
										y = c(GMDF$lwr.ci[l], GMDF$mean[l]), col = COLs[c], lwd = lwd)
							lines(x = c(shifted.x-0.1, shifted.x +0.1), 
										y = c(GMDF$lwr.ci[l], GMDF$lwr.ci[l]), col = COLs[c], lwd = lwd)
							lines(x = c(shifted.x, shifted.x), 
										y = c(GMDF$upr.ci[l], GMDF$mean[l]), col = COLs[c], lwd = lwd)
							lines(x = c(shifted.x-0.1, shifted.x +0.1), 
										y = c(GMDF$upr.ci[l], GMDF$upr.ci[l]), col = COLs[c], lwd = lwd)
						}
						if(lineColDark == TRUE){
							lines(x = c(shifted.x, shifted.x), 
										y = c(GMDF$lwr.ci[l], GMDF$mean[l]), col = COLsDark[c], lwd = lwd)
							lines(x = c(shifted.x-0.1, shifted.x +0.1), 
										y = c(GMDF$lwr.ci[l], GMDF$lwr.ci[l]), col = COLsDark[c], lwd = lwd)
							lines(x = c(shifted.x, shifted.x), 
										y = c(GMDF$upr.ci[l], GMDF$mean[l]), col = COLsDark[c], lwd = lwd)
							lines(x = c(shifted.x-0.1, shifted.x +0.1), 
										y = c(GMDF$upr.ci[l], GMDF$upr.ci[l]), col = COLsDark[c], lwd = lwd)
						}
					}
					
					shiftedX <- GMDF$x.axis - ((length(categories) * xShiftFactor)/2) + (c-1) * xShiftFactor
					if(lineColDark == FALSE){
						lines(x = shiftedX, y = GMDF$mean, col = COLs[c], lwd = lwd*2)
					}
					if(lineColDark == TRUE){
						lines(x = shiftedX, y = GMDF$mean, col = COLsDark[c], lwd = lwd*2)
					}
				} # if(geometricMean == TRUE)
				
				legendX <- 3+length(colNames) - (3+length(colNames))/3
				# legendY <- ylim[2] - (ylim[2] - ylim[1])/10*c
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
					legendX <- 2+length(colNames) - (2+length(colNames))/4
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
