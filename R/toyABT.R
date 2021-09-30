# source: 20210924_AntibodyTiters_1.txt
toyABT <- function(fileName = "toy.xlsx", pmax = 7, patients = 20, outsiderPercent = 2, NaPercent = 10){
	# pmax should be less than 18
	# require(openxlsx)
	toyDF <- emptyABT(pmax = pmax, returnDF = TRUE)
	toyDF[2:(patients+1),] <- NA

	patientIDs <- paste("patient ", 1:patients, sep = "")
	toyDF[["ID"]][2:(patients+1)] <- patientIDs

	# days
	colIndexs <- c(2, 4, 5, 7, 8, seq(10, (10+(pmax-4)*2), 2))

	for(c in 1:5){
		colIndex <- colIndexs[c]
		DAY <- as.integer(as.Date(toyDF[[colIndex]][1], format = "%Y%m%d"))
		for(p in 1:patients){
			temp <- as.character(as.Date(DAY + sample(-10:10, size = 1), 
					origin = "1970-01-01"))
			temp <- paste(strsplit(temp, split = "-")[[1]], collapse = "")
			toyDF[[colIndex]][(p+1)] <- temp
		}
	}
	for(c in 6:length(colIndexs)){
		colIndex <- colIndexs[c]
		DAY <- as.integer(as.Date(toyDF[[colIndex]][1], format = "%Y%m%d"))
		for(p in 1:patients){
			temp <- as.character(as.Date(DAY + sample(-30:30, size = 1), 
					origin = "1970-01-01"))
			temp <- paste(strsplit(temp, split = "-")[[1]], collapse = "")
			toyDF[[colIndex]][(p+1)] <- temp
		}
	}

	for(p in 1:patients){
		temp <- as.integer(toyDF[p+1, colIndexs])
		temp <- as.character(temp[order(temp, decreasing = FALSE)])
		toyDF[p+1, colIndexs] <- temp
	}

	# scores
	colIndexs <- c(3, 6, 9, seq(11, (11+(pmax-4)*2), 2))
	smax <- max(toyDF[1, colIndexs], na.rm = TRUE)

	for(c in 1:length(colIndexs)){
		colIndex <- colIndexs[c]
		score <- toyDF[[colIndex]][1]
		scores <- sample((score/2):(score*2), size = patients, replace = TRUE)
		toyDF[[colIndex]][2:(patients+1)] <- scores
	}

	# Already high before vaccination
	outsiderIndex <- sample(1:patients, size = ceiling(patients * 0.02 * outsiderPercent))
	for(o in 1:length(outsiderIndex)){
		patientScores <- as.numeric(toyDF[1+outsiderIndex[o], colIndexs])
		patientScores[1] <- sample((smax/2):(smax*2), size = 1)
		patientScores[2] <- patientScores[2] + patientScores[1]
		patientScores[3] <- patientScores[2] + patientScores[1]
		toyDF[1+outsiderIndex[o], colIndexs] <- patientScores
	}

	# No increase after vaccination
	outsiderIndex <- sample(1:patients, size = ceiling(patients * 0.02 * outsiderPercent))
	for(o in 1:length(outsiderIndex)){
		patientScores <- as.numeric(toyDF[1+outsiderIndex[o], colIndexs])
		patientScores[1:length(patientScores)] <- sample(0:2, 
			size = length(patientScores), replace = TRUE)
		toyDF[1+outsiderIndex[o], colIndexs] <- patientScores
	}

	# NA
	colIndexsC <- c(2, 5, 8, seq(10, (10+(pmax-4)*2), 2))
	colIndexsS <- c(3, 6, 9, seq(11, (11+(pmax-4)*2), 2))
	NaIndex <- sample(1:patients, size = ceiling(patients * 0.02 * NaPercent))
	for(n in 1:length(NaIndex)){
		x <- sample(1:length(colIndexsC), size = 1)
		toyDF[1+NaIndex[n], colIndexsC[x]] <- NA
		toyDF[1+NaIndex[n], colIndexsS[x]] <- NA
	}

	write.xlsx(toyDF, file = fileName, overwrite = TRUE)
}
